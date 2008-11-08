#!/usr/bin/env ruby

s = []

compress = "gzip"

bm = "./bm_01"

def silent(*cmds)
  c, cs = *cmds
  cmd = ["#{c} 2>/dev/null", cs].compact.join(" | ")
  `#{cmd}`
end


50.times do |i|
  extprot = silent("./bm_01 --seed #{i} -n 1 --dumpbin").size
  xml = silent("./bm_01 --seed #{i} -n 1 --xml").size
  xmlcomp = silent("./bm_01 --seed #{i} -n 1 --xml", compress).size
  extprotcomp = silent("./bm_01 --seed #{i} -n 1 --dumpbin", compress).size

  extprotcomp = "NO GAIN" if extprotcomp >= extprot

  s << [extprot, xml, xmlcomp, extprotcomp]
end

s.sort.each{|a| puts a.join("\t\t")}
