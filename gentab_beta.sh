#!/bin/bash
# Gentab
# It is a human readable table generator using awk and perl.
# The output is an xls file.
# www.github.com/ennaniux/R_surveys


FILE=$1

MYAWK0=$(
awk 'BEGIN {FS=",";OFS=","; date=strftime("%Y-%m-%d"); {print "\nWelcome to gentab\n"}} ; $2 ~ /NAME|AP4_3_3.01/ {print $1, $2, $3, $4, $6, $10, $12, $16, $18} END {print "Done! \n Generated by gentab \n date " date; print " github.com/ennaniux"}'  "$FILE" 
)


Find the unique values of a culumn
awk -F"," 'NR > 2{ a[$2]++ } END { for (i in a) { for (i print a[i], i } }' 

grep 



MYAWK=$(
    awk 'BEGIN {FS=",";OFS=","};  {print $1, $2, $3, $4, $6, $10, $12, $16, $18}'  "$FILE" 
)

# echo "$MYAWK"

export MYAWK0
export MYAWK

perl <<'__HERE__' 
print "this is in perl $ENV{MYAWK0}\n";

use strict;  
use warnings;   

use Excel::Writer::XLSX;

my @data = split(/\n/,$ENV{MYAWK});
my $exp_size = scalar @data; 
print "Size: $exp_size \n"; 

my $scale_1 = 0.02;
my $scale_2 = 0.03;

my ($x,$y) = (0,0);


my $workbook= Excel::Writer::XLSX->new( 'Test.xls' );  
my $worksheet = $workbook->add_worksheet();  


# Light red fill with dark red text.
my $format1 = $workbook->add_format(bg_color => 'white');

# Light yellow fill with dark yellow text.
my $format2 = $workbook->add_format(bg_color => '#eae2b7');

# Green fill with dark green text.
my $format3 = $workbook->add_format(bg_color => '#f77f00');


for my $i (@data){
my @input = split(",",$i);

$worksheet->write($x++, $y, \@input , $format1);
}
### Conditional formatting 1


$worksheet->conditional_formatting( 1,3, $exp_size, 4,
    {
        type     => 'formula',
        criteria => '=$E2 > 0.25',
        format   => $format3,
    }
);


$worksheet->conditional_formatting( 1,3, $exp_size, 4,
    {
        type     => 'formula',
        criteria => '=$E2 > 0.15',
        format   => $format2,
    }
);




# ### Conditional formatting 2



$worksheet->conditional_formatting( 1,5, $exp_size, 6,
    {
        type     => 'formula',
        criteria => '=$G2 > 0.25',
        format   => $format3,
    }
);


$worksheet->conditional_formatting( 1,5, $exp_size, 6,
    {
        type     => 'formula',
        criteria => '=$G2 > 0.15',
        format   => $format2,
    }
);


# ### Conditional formatting 3



$worksheet->conditional_formatting( 1,7, $exp_size, 8,
    {
        type     => 'formula',
        criteria => '=$I2 > 0.25',
        format   => $format3,
    }
);

$worksheet->conditional_formatting( 1,7, $exp_size, 8,
    {
        type     => 'formula',
        criteria => '=$I2 > 0.15',
        format   => $format2,
    }
);



$worksheet->write($exp_size+3, 1, "Generated by gentab");
$workbook->close();


__HERE__

