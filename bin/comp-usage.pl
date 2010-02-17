#!/usr/bin/perl


# nene     :0                            Mon Oct 31 14:17 - 04:15  (13:57)

$date = "";
$minutes = 0;

while (<>) {
  chop;

  s/Mon/E/;
  s/Tue/T/;
  s/Wed/K/;
  s/Thu/N/;
  s/Fri/R/;
  s/Sat/L/;
  s/Sun/P/;


  # read all :0 lines
  if ( m/:0 / ) {
    if ( m/([A-Z]{1}) ([A-Z][a-z]{2}) ([ 0123][0-9]).*\(([0-9]{2}):([0-9]{2})\)/ ) {
        
        if ("$1 $2 $3" ne $date  and   $date ne "" ) {
            $hours = int($minutes / 60);
            $mins = $minutes - ($hours * 60);
            $column = "#" x ($minutes / 30);
            print $date . sprintf(" (%02d:%02d) ", $hours, $mins) . $column . "\n";
            $minutes = 0;
            
            if ($month eq $2  and  ($day + 1) != $3) {
                print "X $month  0 (00:00)\n" x ($3 - $day - 1);
            }
         }
        
        $date = "$1 $2 $3";
        $day = $3;
        $month = $2;
        $minutes += $4 * 60 + $5;
        
        
        
    }
  }
}













