import Diagrams.TwoD.Tilings
import Diagrams.Backend.Cairo.CmdLine

d n t = drawTiling t n n

main = multiMain [ ("t3", d 5 t3)
                 , ("t4", d 5 t4)
                 , ("t6", d 8 t6)
                 , ("t31212", d 15 t31212)
                 , ("t33336L", d 8 t33336L)
                 , ("t33336R", d 8 t33336R)
                 , ("t33344", d 7 t33344)
                 , ("t33434", d 7 t33434)
                 , ("t3464", d 10 t3464)
                 , ("t3636", d 8 t3636)
                 , ("t4612", d 15 t4612)
                 , ("t488", d 10 t488)
                 ]