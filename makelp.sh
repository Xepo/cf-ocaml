rm -f /tmp/tmpfile
touch -r `ls -t | head -n1` /tmp/tmpfile
while(true) 
do 
     sleep 1;
     if [ "$(find * -newer /tmp/tmpfile)" != '' ]
     then
          touch -r `ls -t | head -n1` /tmp/tmpfile
          make 
          touch -r `ls -t | head -n1` /tmp/tmpfile
     fi
done
