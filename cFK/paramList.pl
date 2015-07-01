#!/usr/bin/perl
#
use warnings;
#use Data::Dump qw(dump);

sub logspace{
  my ($start, $end, $num) = @_;
  #my $scale=0;
  #if ($end!=0) {
  #  $scale= ($end/$start)^(1/$num);
  #}
  #my $logstart = log($start);
  #my $logend = log($end);
  my @logVals = (0)x$num;
  my $den = $num;
  if ($end != 0) {
    if ($num == 1) {$den=2}
    my $spacing = ( log10($end) - log10($start) )/($den-1);
    my @exponents = map {log10($start) + $_*$spacing} (0 .. ($num-1));
    # print "exponents: @exponents";
    @logVals = map { 10**$_ } (@exponents);
  }
  return @logVals
}
sub log10 {
  my $n = shift;
  return log($n)/log(10);
}

# set to zero to change values with divvyProcs.sh
my $kstart=0.00e0; #(4,4) 47, (5,5) 28.9, polymer 14
my $kend=0.00e0;
my $knum=1;
#my @k=logspace($kstart,$kend,$knum);
#my @k=(2,2.5,3,4,5,6,8,10,12,16,20,24,32,48,64,96,128, 160, 192);
my @k=(2,2.5,3,4,5,6,8,10,12,16,20,24,32,48,64);
#my @k=(2, 2.5);
my $hstart=1.53e-21; #(4,4) 1.21e-21, (5,5) 5.53e-23
my $hend=1.53e-21;
my $hnum=1;
my @h=logspace($hstart,$hend,$hnum);
my $bgstart=0;
my $bgend=0;
my $bgnum=1;
my @bg=logspace($bgstart,$bgend,$bgnum);
my $etastart=5.0e9;
my $etaend=5.0e9;
my $etanum=1;
my @eta=logspace($etastart,$etaend,$etanum);
my $Tstart=0;
my $Tend=0;
my $Tnum=1;
my @T=logspace($Tstart,$Tend,$Tnum);
my $Gstart=0.00e+00; #1.0e-15; #0.00625e-12; #1e-14;
my $Gend=0.00e+00; #1.0e-13; #4e-14;
my $Gnum=1;
my @G=logspace($Gstart,$Gend,$Gnum);
my $Mstart=3e-26;
my $Mend=3e-26;
my $Mnum=1;
my @M=logspace($Mstart,$Mend,$Mnum);
my $ensstart=0;
my $ensend=0;
my @ens=$ensstart .. $ensend;
my $iter=1;
my $iterations=1;

#dump "h: @h";
#dump "G: @G";

#print "array G length " . (@G+0) . "\n";
my $alllength=(@ens+0)*(@k+0)*(@h+0)*(@eta+0)*(@T+0)*(@bg+0)*(@G+0)*(@M+0)*$iterations;
#%alllength=100;
my @enswrite=(0)x$alllength;
my @kwrite=(0)x$alllength;
my @hwrite=(0)x$alllength;
my @bgwrite=(0)x$alllength;
my @etawrite=(0)x$alllength;
my @Twrite=(0)x$alllength;
my @Gwrite=(0)x$alllength;
my @Mwrite=(0)x$alllength;
#print "length of param space: $alllength\n";

my $index=0;
foreach my $hi (@h) {
foreach my $ki (@k) {
foreach my $Ti (@T) {
foreach my $etai (@eta) {
foreach my $bgHi (@bg) {
foreach my $Gi (@G) {
foreach my $Mi (@M) {
foreach my $ensi (@ens) {
  $enswrite[$index]=$ensi+0.0;
  $hwrite[$index]=$hi;
  $roundedh = sprintf("%8.2e", $hi);
  $hwrite[$index]=$roundedh;
  $roundedk = sprintf("%8.2e", $ki);
  $kwrite[$index]=$roundedk;
  $Twrite[$index]=$Ti;
  $roundedeta = int($etai + $etai/abs($etai*2));
  $etawrite[$index]=$roundedeta;
  $bgwrite[$index]=$bgHi;
  $roundedG = sprintf("%8.2e", $Gi);
  $Gwrite[$index]=$roundedG;
  $Mwrite[$index]=$Mi;
  $index=$index+1;
} } } } } } } }
#dump "hwrite: @hwrite";


#     'ens = '    . $iter  . "\n" . 
my $out = "&cFKconstants\n" .
     'ens = '  . join(" ",@enswrite) . "\n" . 
     'k = '    . join(" ",@kwrite)   . "\n" . 
     'h = '    . join(" ",@hwrite)   . "\n" .
     'bgH = '  . join(" ",@bgwrite)  . "\n" .
     'Temp = ' . join(" ",@Twrite)   . "\n" .
     'eta = '  . join(" ",@etawrite) . "\n" .
     'G = '    . join(" ",@Gwrite)   . "\n" .
     'M = '    . join(" ",@Mwrite)   . "\n/";
print $out;
print "\n";
open (SWEEP, '>paramList.in');
print SWEEP $out;
close SWEEP;
