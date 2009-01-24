
#include <ruby.h>
#include <rubyio.h>
#include <stdio.h>
#include <alloca.h>
#include <arpa/inet.h>

static VALUE rb_HTuple_c, rb_Enum_c, rb_Tuple_c, rb_Assoc_c;
static ID id_or, id_shl, id_read, id_readchar;

#define VINT 0
#define BITS8 2
#define BITS32 4
#define BITS64_LONG 6
#define BITS64_FLOAT 8
#define ENUM 10
#define TUPLE 1
#define BYTES 3
#define HTUPLE 5
#define ASSOC 7

#define Raise_EOF rb_eof_error()
#define Read_vint(fp, n) \
    do { if(file_read_vint((fp), (n)) != 0) Raise_EOF; } while(0);

#define IO_Read_vint(io) (io_read_vint(io))

static unsigned char *
read_vint(unsigned char *ptr, unsigned char *end, unsigned int *dst)
{
 unsigned int b;
 unsigned int x = 0, e = 0;
 if(ptr >= end) return NULL;
 b = *ptr++;
 if(b < 128) {
     *dst = b;
     return ptr;
 }
 if(ptr >= end) return NULL;
 while(b >= 128) {
     x += (b - 128) << e;
     e += 7;
     b = *ptr++;
     if(ptr >= end) return NULL;
 }
 *dst = x + (b << e);
 return ptr;
}

static int
file_read_vint(FILE *fp, unsigned int *n)
{
 int b;
 unsigned int x = 0, e = 0;

 b = fgetc(fp);
 if(b == EOF) return -1;
 if(b < 128) {
     *n = b;
     return 0;
 }
 while(b >= 128) {
     x += (b - 128) << e;
     e += 7;
     b = fgetc(fp);
     if(b == EOF) return -1;
 }

 *n = x + (b << e);
 return 0;
}

static int
io_read_vint(VALUE io)
{
 VALUE b;
 unsigned int x = 0, e = 0;

 b = FIX2INT(rb_funcall(io, id_readchar, 0));
 if(b < 128) return b;
 while(b >= 128) {
     x += (b - 128) << e;
     e += 7;
     b = FIX2INT(rb_funcall(io, id_readchar, 0));
 }

 return (x + (b << e));
}

static unsigned char *
do_read_value(unsigned char *ptr, unsigned char *end, VALUE *dst)
{
 unsigned int prefix, n, tag, len, nelms, wtype;

#define Read_check(dst)  \
   do { ptr = read_vint(ptr, end, dst); if(!ptr) return NULL; } while(0);

 if(!ptr || ptr >= end) return NULL;
 ptr = read_vint(ptr, end, &prefix);
 if(!ptr) return NULL;
 tag = prefix >> 4;
 wtype = prefix & 0xF;
 switch(wtype) {
     case VINT:
	 Read_check(&n);
	 *dst = INT2NUM((int)n >> 1 ^ -(n & 1));
	 break;
     case BITS8:
	 *dst = INT2FIX(*ptr++);
	 break;
     case BITS32:
	 if(ptr + 4 > end) return NULL;
	 /* FIXME: endianness */
	 *dst = INT2NUM(*(int32_t *) ptr);
	 ptr += 4;
	 break;
     case BITS64_LONG:
	 if(ptr + 8 > end) return NULL;
	 {
	     /* FIXME: endianness */
	     VALUE l, h;
	     l = INT2NUM(*(int32_t *) ptr);
	     h = INT2NUM(*(int32_t *) (ptr + 4));
	     *dst = rb_funcall(l, id_or, 1,
		               rb_funcall(h, id_shl, 1, INT2FIX(32)));
	     ptr += 8;
	 }
	 break;
     case BITS64_FLOAT:
	 if(ptr + 8 > end) return NULL;
	 /* FIXME: endianness */
	 *dst = rb_float_new(*(double *)ptr);
	 ptr += 8;
	 break;
     case ENUM:
	 *dst = rb_obj_alloc(rb_Enum_c);
	 rb_iv_set(*dst, "@tag", INT2FIX(tag));
	 break;
     case TUPLE:
     case HTUPLE:
	 Read_check(&len);
	 end = ptr + len;
	 Read_check(&nelms);
	 *dst = rb_ary_new2(nelms);
	 rb_iv_set(*dst, "@tag", INT2FIX(tag));
	 RBASIC(*dst)->klass = (wtype == TUPLE) ? rb_Tuple_c : rb_HTuple_c;
	 for(n = 0; ptr && (n < nelms); n++) {
	     /* we enlarge the array before reading the val because the GC
	      * might be triggered */
	     RARRAY(*dst)->len++;
	     RARRAY(*dst)->ptr[n] = Qnil;
	     ptr = do_read_value(ptr, end, RARRAY(*dst)->ptr + n);
	     if(!ptr) return NULL;
	 };
	 break;
     case BYTES:
	 Read_check(&len);
	 if(len < 0 || ptr + len < 0 || ptr + len > end) return NULL;
	 *dst = rb_str_new(ptr, len);
	 ptr += len;
	 break;
     case ASSOC:
	 Read_check(&len);
	 end = ptr + len;
	 Read_check(&nelms);
	 *dst = rb_hash_new();
	 rb_set_iv(*dst, "tag", INT2FIX(tag));
	 RBASIC(*dst)->klass = rb_Assoc_c;
	 for(n = 0; ptr && n < nelms; n++) {
	     VALUE k, v;
	     ptr = do_read_value(ptr, end, &k);
	     if(!ptr) return NULL;
	     ptr = do_read_value(ptr, end, &v);
	     if(!ptr) return NULL;
	     rb_hash_aset(*dst, k, v);
	 };
	 break;
     default:
	 rb_raise(rb_eRuntimeError, "Unknown wire type.");
 }

 return ptr;
#undef Read_check
}

VALUE
extprot_read_value(VALUE self, VALUE io)
{
 OpenFile *fptr;
 FILE *fp;
 unsigned char *buf;
 unsigned char *end;
 unsigned int prefix;
 unsigned int tag;
 unsigned int len;
 unsigned int nelms;
 VALUE ret;
 unsigned int i;

 if(TYPE(io) == T_FILE) {
     GetOpenFile(io, fptr);
     fp = GetReadFile(fptr);
     Read_vint(fp, &prefix);
     if((prefix & 0xF) != 1)
	 rb_raise(rb_eRuntimeError, "Expected message (Tuple wire type); prefix: %x",
		  prefix);
     Read_vint(fp, &len);
     buf = alloca(len);
     if(fread(buf, 1, len, fp) != len) Raise_EOF;
 } else {
     VALUE str;
     prefix = io_read_vint(io);
     if(prefix & 0xF != 1) Raise_EOF;
     len = io_read_vint(io);
     str = rb_funcall(io, id_read, 1, INT2FIX(len));
     if (NIL_P(str)) Raise_EOF;
     StringValue(str);
     if (RSTRING(str)->len != len) Raise_EOF;
     buf = RSTRING(str)->ptr;
 }

 end = buf + len;
 buf = read_vint(buf, end, &nelms);
 if(!buf) rb_raise(rb_eRuntimeError, "Couldn't read number of fields in message.");

 ret = rb_ary_new2(nelms);
 for(i = 0; buf && i < nelms; i++) {
     /* enlarge before reading */
     RARRAY(ret)->len++;
     RARRAY(ret)->ptr[i] = Qnil;
     buf = do_read_value(buf, end, RARRAY(ret)->ptr + i);
 }
 if(!buf) return Qnil;
 RBASIC(ret)->klass = rb_Tuple_c;
 return ret;
}

void
Init_extprot_decoder()
{
 int status;
 VALUE extprot_m;

 id_or = rb_intern("|");
 id_shl = rb_intern("<<");
 id_read = rb_intern("read");
 id_readchar = rb_intern("readchar");

 extprot_m = rb_eval_string_protect("::Extprot", &status);
 if(status) extprot_m = rb_define_module("Extprot");

 rb_HTuple_c = rb_eval_string_protect("::Extprot::HTuple", &status);
 if(status) rb_HTuple_c = rb_define_class_under(extprot_m, "HTuple", rb_cArray);

 rb_Tuple_c = rb_eval_string_protect("::Extprot::Tuple", &status);
 if(status) rb_Tuple_c = rb_define_class_under(extprot_m, "Tuple", rb_HTuple_c);

 rb_Enum_c = rb_eval_string_protect("::Extprot::Enum", &status);
 if(status) rb_Enum_c = rb_define_class_under(extprot_m, "Enum", rb_cObject);

 rb_Assoc_c = rb_eval_string_protect("::Extprot::Assoc", &status);
 if(status) rb_Assoc_c = rb_define_class_under(extprot_m, "Assoc", rb_cHash);

 rb_define_singleton_method(extprot_m, "read_value", extprot_read_value, 1);
}
