case "$1" in
   start)
      perl wsdemo.pl
      ;;
   stop)
      killall perl      
      ;;
   *)
      echo "Usage $0 start|stop"
      ;;
esac
