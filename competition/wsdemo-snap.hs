import Control.Monad
import Snap.Http.Server
import Network.WebSockets
import Network.WebSockets.Snap

echo :: Request -> WebSockets Hybi00 ()
echo req = acceptRequest req >> forever (receive >>= send)

main :: IO ()
main = quickHttpServe $ runWebSocketsSnap echo
