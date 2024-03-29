#!/bin/bash
# Daniel Ballesteros-Chávez & Jorge Becerril-Cejudo.
# Usage: ./gentab_gamma.sh

# <style type=\"text/css\"> tr:nth-child(odd) {background-color: #e2e2e2;}  tr:first-child {font-weight: bold}  tr:hover {background-color: #d0c6e5;}</style>

# td, th {
#   border: 1px solid #dddddd;
#   text-align: left;
#   padding: 8px;
# }

# tr:nth-child(even) {
#   background-color: #dddddd;
# }


FILE=$1
FILENAME="${FILE:0: -3}html"

rm "${FILENAME}"

# FILE2=$2

# Number of rows in $FILE
ROWS=$(cat $FILE | wc -l)

TITLE2=$(awk -F"," 'NR==2 {print $22}' $FILE | tr -d '"')
TITLE3=$(awk -F"," 'NR==2 {print $23}' $FILE | tr -d '"')
#TITLE2="This is just an Example 00"
:"${TITLE:=Título: $TITLE2}"



REPORT="
<!DOCTYPE html>
<html>
<head>
<style>
table {
  font-family: arial, sans-serif;
  border-collapse: collapse;
  width: 70%;
}

td, th {border: 1px solid #dddddd; text-align: left; padding: 8px;}

/* tr:nth-child(even) { */
/*  background-color: #dddddd; }*/

</style>
<style type=\"text/css\"> tr:first-child {font-weight: bold; background-color: #e2e2e2;} </style>

</head>
<body>

<h2>$TITLE</h2>
<h3>$TITLE3</h3>
<h3> Created by: gentab (gamma) </h3>
<h3> ennaniux</h3>

"

echo "$REPORT" >> "$FILENAME"

# The counter starts from the second row (assuming headers in template_data.csv)
for id in  `seq 1 $ROWS`; do

# echo "$id"

NAME=$(awk -F"," 'NR=='$id' {print $2}' $FILE | tr -d '"')

LEVELS=$(awk -F"," 'NR=='$id' {print $3}' $FILE | tr -d '"')

DENOMINATOR=$(awk -F"," 'NR=='$id' {if ($4 !=($4+0)) print $4; else printf "%15.0f", $4}' $FILE | tr -d '"')
DFORMAT=$(awk -F"," 'NR=='$id' {if ($6 != ($6 + 0 )) print ""; else if ($6 > 0.25 ) print "background-color:#f77f00"; else if( $6 >= 0.15 && $6 <=0.25) print "background-color:#eae2b7"; else if($6 < 0.15) print "background-color:#FFFFFF"}' $FILE)

TOTALS=$(awk -F"," 'NR=='$id' {if ($10 !=($10+0)) print $10; else printf "%15.0f", $10}' $FILE | tr -d '"')
TFORMAT=$(awk -F"," 'NR=='$id' {if ($12 != ($12 + 0 )) print ""; else if ($12 > 0.25 ) print "background-color:#f77f00"; else if( $12 >= 0.15 && $6 <=0.25) print "background-color:#eae2b7"; else if($12 < 0.15) print "background-color:#FFFFFF"}' $FILE)

MEANS=$(awk -F"," 'NR=='$id' {if ($16 !=($16+0)) print $16; else printf "%15.1f", $16 }' $FILE | tr -d '"')
MFORMAT=$(awk -F"," 'NR=='$id' {if ($18 != ($18 + 0 )) print ""; else if ($18 > 0.25 ) print "background-color:#f77f00"; else if( $18 >= 0.15 && $6 <=0.25) print "background-color:#eae2b7"; else if($18 < 0.15) print "background-color:#FFFFFF"}' $FILE)


# In case you have your template in a file
# REPORT="$(cat ${FILE2})"
# REPORT=$(eval "echo \"${REPORT}\"")

if [ "$id" == 1 ]; then 

REPORT="
<table>
  <tr>
    <th>$NAME</th>
    <th>$LEVELS</th>
    <th  $DFORMAT>$DENOMINATOR</th>
    <th  $TFORMAT>$TOTALS</th>
    <th  $MFORMAT>$MEANS</th>
  </tr>"

echo "$REPORT"
echo "$REPORT" >> "$FILENAME"

elif [ "$id" == "$ROWS" ]; then
    
REPORT="
 <tr>
<td>$NAME</td>
<td>$LEVELS</td>
<td style=\" $DFORMAT ; text-align:right\">$DENOMINATOR</td>
<td style=\" $TFORMAT ; text-align:right\">$TOTALS</td>
<td style=\" $MFORMAT ; text-align:right\">$MEANS</td>
</tr>
</table>

</body>
</html>
"
echo "$REPORT"
echo "$REPORT" >> "$FILENAME"


else

REPORT="
 <tr>
<td>$NAME</td>
<td>$LEVELS</td>
<td style=\" $DFORMAT ; text-align:right\">$DENOMINATOR</td>
<td style=\" $TFORMAT ; text-align:right\">$TOTALS</td>
<td style=\" $MFORMAT ; text-align:right\">$MEANS</td>
</tr>
"

echo "$REPORT"
echo "$REPORT" >> "$FILENAME"

fi

done 

