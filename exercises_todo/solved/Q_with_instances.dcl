definition module Q_with_instances

import StdEnv

::	Q  

//// Instances for Q type

mkQ :: Int Int -> Q

instance == Q

instance < Q

instance + Q

instance - Q

instance zero Q

instance * Q

instance / Q

instance one Q

instance abs Q

instance sign Q

instance ~ Q

instance fromInt Q

instance toInt Q

isIntQ :: Q -> Bool

instance toReal Q

instance toString Q

