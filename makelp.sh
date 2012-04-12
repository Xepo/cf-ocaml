dotouch()  {
     touch -r `ls -t | head -n1` /tmp/tmpfile
}

rm -f /tmp/tmpfile
dotouch
while(true) 
do 
     sleep 1;
     if [ "$(find * -newer /tmp/tmpfile)" != '' ]
     then
          dotouch
          make $@ || true
          dotouch
     fi
done
