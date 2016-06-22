module HGE2D.Physical where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Instances

--------------------------------------------------------------------------------

---TODO not fully implemented
---TODO rename
---TODO rename class to HasPhysicalObject
applyPhysics :: (IsPhysicalObject a) => Millisecond -> a -> a
applyPhysics ms x = setPhys newPo x
  where
    newPo = (applyVel . applyAcc . applyRotVel . applyRotAcc . applyDrag . applyRotDrag) (getPhys x) ---TODO more consistent order acc > vel > abs

    applyVel po = moveBy (dX, dY) po ---TODO define more general for reuse
      where
        dX = (fromIntegral ms) * (velX $ physicalVel po)
        dY = (fromIntegral ms) * (velY $ physicalVel po)

    applyAcc po = po { physicalVel = (Velocity vX vY) } ---TODO see above
      where
        vX = (velX $ physicalVel po) + (fromIntegral ms) * (accX $ physicalAcc po)
        vY = (velY $ physicalVel po) + (fromIntegral ms) * (accY $ physicalAcc po)

    applyRotVel po = po { physicalRot = fixedRot } ---TODO see above
      where
        fixedRot | newRot > 2.0 * pi = newRot - 2.0 * pi
                 | newRot < 0        = newRot + 2.0 * pi
                 | otherwise         = newRot
        newRot = (physicalRot po)  + dRot
        dRot = (fromIntegral ms) * (physicalRotSpeed po)

    applyRotAcc po = po { physicalRotSpeed = newRotSpeed } ---TODO see above
      where
        newRotSpeed = (physicalRotAcceleration po) + dRotSpeed
        dRotSpeed = (fromIntegral ms) * (physicalRotAcceleration po)

    applyDrag = id ---TODO
    applyRotDrag = id ---TODO
