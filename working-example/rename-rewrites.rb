Dir['*.sail'].each do |filename|
  if /^intermediate_rewrite_(\d+)_(\w+)\.sail$/ =~ filename
    index, description = $1, $2
    new_filename = "rewrite-#{index.rjust(2, '0')}-#{description}.sail"
    File.rename filename, new_filename
  end
end
