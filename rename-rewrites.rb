$files = []

Dir['*.sail'].each do |filename|
  if /^intermediate_rewrite_(\d+)_(\w+)\.sail$/ =~ filename
    index, description = $1, $2
    new_filename = "rewrite-#{index.rjust(2, '0')}-#{description}.sail"
    File.rename filename, new_filename
    $files << new_filename
  end
end

$files.sort!

$files.each_cons(2).with_index do |(original, updated), index|
  filename = "delta-#{(index+1).to_s.rjust(2, '0')}.diff"
  command = "diff #{original} #{updated} >> #{filename}"
  
  IO.write filename, <<~END
  #{command}
  END
  
  `#{command}`
end
