case "$1" in
   start)
      go run wsdemo.go
      ;;
   stop)
      ;;
   *)
      echo "Usage $0 start|stop"
      ;;
esac
