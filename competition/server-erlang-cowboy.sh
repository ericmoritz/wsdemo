case "$1" in
   start)
      erl -pa ../ebin ../deps/*/ebin +K true -s wsdemo
      ;;
   stop)
      killall beam.smp
      ;;
   *)
      echo "Usage $0 start|stop"
      ;;
esac
