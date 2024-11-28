#!/usr/bin/env ruby

directories = Dir['*'].select do |entry|
  File.directory? entry
end


table = Hash.new { |h, k| h[k] = [] }

directories.each do |directory|
  Dir.chdir directory do
    Dir['*.diff'].each do |file|
      if File.size(file) != 0
        file =~ /delta-(\d+-.*)\.diff/ or abort "Could not parse #{file}"
        tag = $1
        
        table[tag] << directory
      end
    end
  end
end


table.keys.sort.each do |tag|
  directories = table[tag]
  
  puts tag
  directories.each do |directory|
    puts "  #{directory}"
  end
end
