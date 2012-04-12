tmp="/tmp/tmptimefile.$$"
dotouch()  {
     touch -r `ls -t | head -n1` $tmp
}

rm -f $tmp
dotouch
while(true) 
do 
     sleep 1;
     if [ "$(find * -newer $tmp)" != '' ]
     then
          dotouch
          make $@ || true
          echo .
          dotouch
     fi
done
