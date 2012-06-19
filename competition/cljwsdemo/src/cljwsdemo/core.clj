(ns cljwsdemo.core (:gen-class))
(use 'aleph.http 'lamina.core)

(defn ws-handler [ch req]
  (siphon ch ch))

(defn -main
  "Starts the websocket server"
  [& args]
  (start-http-server ws-handler {:port 8080 :websocket true}))
