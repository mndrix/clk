use inc::Module::Install;

# Define metadata
name     'App-Clk';
all_from 'lib/App/Clk.pm';

# Core dependencies
requires 'File::Path';
requires 'File::Temp';
requires 'FindBin';
requires 'IPC::Open2';
requires 'IPC::Open3';
requires 'POSIX';
requires 'Time::Local';

# Non-core dependencies
requires 'Digest::SHA1';
requires 'File::chdir';

# documentation dependencies
requires 'Pod::Simple::HTML';

# testing dependencies
test_requires 'Test::Builder';

# scripts
install_script 'clk';
install_script 'clk-cat-entry';
install_script 'clk-entry-search';
install_script 'clk-fsck';
install_script 'clk-in';
install_script 'clk-list';
install_script 'clk-out';
install_script 'clk-rm';
install_script 'clk-store-entry';

tests_recursive;
WriteAll;
