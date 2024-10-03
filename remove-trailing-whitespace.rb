#!/usr/bin/env ruby

require 'pathname'
require 'find'


Find.find('.') do |path|
  unless path =~ %r{^./_build}
    path = Pathname.new path

    if [ '.ml', '.mli', '.sail' ].include? path.extname
      path
        .read
        .gsub(%r{ +\n}, "\n")
        .then { |contents| path.write contents }
    end
  end
end
