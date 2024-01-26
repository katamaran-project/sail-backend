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
  updated =~ /rewrite-\d+-(.*)\.sail/ or abort "could not parse #{updated}"
  rewrite_title = $1

  filename = "delta-#{(index+1).to_s.rjust(2, '0')}-#{rewrite_title}.diff"
  command = "diff #{original} #{updated} >> #{filename}"

  IO.write filename, <<~END
  #{command}
  END

  `#{command}`
end
