#!/bin/bash
# Rodrigo Dávila Figueroa - Daniel Ballesteros-Chávez
# Gentab
# It is a human readable table generator using awk and perl.
# The output is an xls file.
# www.github.com/ennaniux/R_surveys


FILE=$1

# MYAWK0=$(
# awk 'BEGIN {FS=",";OFS=","; date=strftime("%Y-%m-%d"); {print "\nWelcome to gentab\n"}} ; $2 ~ /NAME|AP4_3_3.01/ {print $1, $2, $3, $4, $6, $10, $12, $16, $18} END {print "Done! \n Generated by gentab \n date " date; print " github.com/ennaniux"}'  "$FILE" 
# )

MYAWK=$(
    awk 'BEGIN {FS=",";OFS=","};  {print $1, $2, $3, $4, $6, $10, $12, $16, $18}'  "$FILE" | tr -d '"'
)


# echo "$MYAWK"


export MYAWK0
export MYAWK
# export scale_1
# export scale_2



perl <<'__HERE__' 
print "this is in perl $ENV{MYAWK}\n";

use strict;  
use warnings;   

use Excel::Writer::XLSX;

# Reading the CSV
my @data = split(/\n/,$ENV{MYAWK});
my $exp_size = scalar @data; 
print "Size: $exp_size \n"; 


my ($x,$y) = (0,0);


my $workbook= Excel::Writer::XLSX->new( 'myExcel.xls' );  
my $worksheet = $workbook->add_worksheet();  


# Header Format
my $format0 = $workbook->add_format(bg_color => 'orange', bold => 1 , align => 'vcenter',size  => 12 );

# This is the default backgound format empty
my $format1 = $workbook->add_format();

# Background scale 1
my $format2 = $workbook->add_format(bg_color => '#eae2b7');

# Background scale 2
my $format3 = $workbook->add_format(bg_color => '#f77f00');

# Numeric formats
my $format_nT = $workbook->add_format( num_format => '# ### ##0');
my $format_nC = $workbook->add_format( num_format => '# ##0.00');

# Default cell width
$worksheet->set_column( 0,8, 20);


for my $i (@data){
if($i eq $data[0]){
my @input = split(",",$i);
$worksheet->write($x++, $y, \@input , $format0);
$worksheet->write(0, 0, "Nr" , $format0);
}else
{
my @input = split(",",$i);
$worksheet->write($x, $y,   $input[0] , $format1);
$worksheet->write($x, $y+1, $input[1] , $format1);
$worksheet->write($x, $y+2, $input[2] , $format1);
$worksheet->write($x, $y+3, $input[3] , $format_nT);
$worksheet->write($x, $y+4, $input[4] , $format_nC);
$worksheet->write($x, $y+5, $input[5] , $format_nT);
$worksheet->write($x, $y+6, $input[6] , $format_nC);
$worksheet->write($x, $y+7, $input[7] , $format_nC);
$worksheet->write($x, $y+8, $input[8] , $format_nC);
$x++;
}
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




### Conditional formatting 2

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


### Conditional formatting 3

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




$worksheet->merge_range_type('string', $exp_size+3, 1,$exp_size+3, 3, "The coefficient of variation (CV) is between .15 and 0.25", $format2);
$worksheet->merge_range_type('string', $exp_size+4, 1,$exp_size+4, 3, "The coefficient of variation (CV) is bigger 0.25", $format3);

$worksheet->write($exp_size+6, 1, "Generated by gentab");

$workbook->close();

__HERE__

