#!/usr/bin/env ruby
require 'fileutils'
SKIP_PATTERNS=[/^\./, /^bin$/, /^lib$/, /^util$/]
install_dir = File.expand_path(File.join(File.dirname(__FILE__), ".."))
home = ENV['HOME']
Dir.new(install_dir).each do |filename|
 home_file_name =  File.join(home, ".#{filename}")
 checked_in_file_name = File.join(install_dir, filename)
 FileUtils.ln_sf(checked_in_file_name,home_file_name) unless SKIP_PATTERNS.map {|pattern| filename.match pattern}.compact.any? ||File.exists?(home_file_name)
end


