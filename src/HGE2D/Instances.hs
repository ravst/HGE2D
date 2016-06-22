{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HGE2D.Instances where

import HGE2D.Geometry
import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

instance HasBoundingBox BoundingBox where
    getBB = id

instance HasBoundingBox RigidBody where
    getBB = rigidBB

instance HasBoundingBox PhysicalObject where
    getBB = physicalBB

--------------------------------------------------------------------------------

instance Positioned RealPosition where
    getPos = id
    getX = fst
    getY = snd

instance Positioned RigidBody where
    getPos = getPos . rigidPos
    getX = getX . rigidPos
    getY = getY . rigidPos

--------------------------------------------------------------------------------

instance Moveable RealPosition where
    moveBy by pos = (newX, newY)
      where
        newX = fst pos + fst by
        newY = snd pos + snd by
    moveTo to _ = ((fst to), (snd to))

instance Moveable BoundingBox where
    moveBy by bb = BoundingBox { bbMin = newMinPos, bbMax = newMaxPos }
      where
        newMinPos = moveBy by $ bbMin bb
        newMaxPos = moveBy by $ bbMax bb

    moveTo by bb = BoundingBox { bbMin = newMinPos, bbMax = newMaxPos }
      where
        newMinPos = moveTo by $ bbMin bb
        newMaxPos = moveTo by $ bbMax bb

instance Moveable RigidBody where
    moveBy by rb = rb { rigidPos = newPos , rigidBB = newBB }
      where
        newBB = moveBy by (rigidBB rb)
        newPos = moveBy by (rigidPos rb)
    moveTo to rb = rb { rigidPos = newPos, rigidBB = newBB }
      where
        newBB = moveTo to (rigidBB rb)
        newPos = moveTo to $ rigidPos rb
        by = ((fst newPos - getX rb), (snd newPos - getY rb))

instance Moveable PhysicalObject where
    moveBy by po = po { physicalPos = newPos, physicalBB = newBB }
      where
        newBB = moveBy by (physicalBB po)
        newPos = moveBy by (physicalPos po)
    moveTo to po = po { physicalPos = newPos, physicalBB = newBB }
      where
        newBB = moveTo to (physicalBB po)
        newPos = moveTo to (physicalPos po)

--------------------------------------------------------------------------------

instance Dynamic RigidBody where
    moveInTime time rb = rb { rigidPos = newPos , rigidBB = newBB }
      where
        newBB = (rigidBB rb) { bbMin = newMinPos, bbMax = newMaxPos}
        newPos = applyVelocity (rigidPos rb) (rigidVel rb) time
        newMinPos = applyVelocity (bbMin $ rigidBB rb) (rigidVel rb) time
        newMaxPos = applyVelocity (bbMax $ rigidBB rb) (rigidVel rb) time
