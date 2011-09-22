module Language.Lsl.Internal.Physics(
                   bbIntersect,
                   kin,
                   rotDyn,
                   totalTorque,
                   calcAccel,
                   dampZForce,
                   dampForce,
                   dampTorque,
                   primMassApprox,
                   checkIntersections,
                   momentOfInertia,
                   gravC
                   )where

import Language.Lsl.Internal.Math(add3d,axisAngleFromRotation,axisAngleToRotation,diff3d,
                invertQuaternion,mag3d,norm3d,quaternionMultiply,scale3d)
import Language.Lsl.WorldDef(Prim(..),PrimType(..))
import Language.Lsl.Internal.Constants(cPrimTypeBox,cPrimTypeCylinder,cPrimTypeSculpt,cPrimTypeSphere)

rangeOverlap (mn0,mx0) (mn1,mx1) = mn0 >= mn1 && mn0 <= mx1 || mx0 >= mn1 && mx0 <= mx1 || mn0 <= mn1 && mx0 >= mx1
bbIntersect ((mnX0,mnY0,mnZ0),(mxX0,mxY0,mxZ0)) ((mnX1,mnY1,mnZ1),(mxX1,mxY1,mxZ1)) =
   rangeOverlap (mnX0,mxX0) (mnX1,mxX1) &&
   rangeOverlap (mnY0,mxY0) (mnY1,mxY1) &&
   rangeOverlap (mnZ0,mxZ0) (mnZ1,mxZ1)

-- really poor mass approximations, especially when tapering, shearing is involved, or non-box/cyl/spheres are used
boxMassSimple (x,y,z) c1 c2 h = realToFrac 10.0 * x * y * z * c1 * c2 * h
cylMassSimple (x,y,z) c1 c2 h = realToFrac 10.0 * pi * (x/2) * (y/2) * z * c1 * c2 * h
sphMassSimple (x,y,z) c1 c2 h = realToFrac 10.0 * (4/3) * pi * (x/2) * (y/2) * (z/2) * c1 * c2 * h
otherMassSimple :: Floating a => (a,a,a) -> a -> a -> a -> a
otherMassSimple = sphMassSimple

-- volume of a torus = (pi * r^2) * (2 * pi * R) = (x-section-area) * length
-- perimeter of an ellipse = pi * (a + b) * (1 + 3 * h / (10 + sqrt (4 - 3 * h))) where h = (a - b)^2 / (a + b)^2

primMassApprox :: Prim -> Float
primMassApprox (Prim { _primScale = scale, _primTypeInfo = primType }) = primMassApprox' scale primType

primMassApprox' :: (Float,Float,Float) -> PrimType -> Float
primMassApprox' scale primType =
    case primType of
       -- PrimTypeUnknown -> otherMassSimple scale 1 1 1
       PrimType { _primTypeCode = code } ->
           case code of
              c | c == cPrimTypeBox -> boxMassSimple scale c1 c2 h
                | c == cPrimTypeCylinder -> cylMassSimple scale c1 c2 h
                | c == cPrimTypeSphere -> sphMassSimple scale c1 c2 h
                | c == cPrimTypeSculpt -> otherMassSimple scale 1 1 1
                | otherwise -> otherMassSimple scale c1 c2 h
           where c1 = let (x,y,_) = _primCut primType in y - x
                 c2 = let (x,y,_) = _primAdvancedCut primType in y - x
                 h = 1 - _primHollow primType

gravC = -9.80665 :: Float
gravA = (0,0,gravC) :: (Float,Float,Float)
belowGround zoffset (_,_,z) = z + zoffset <= 0

kin t0 t1 t2d zoffs m p0 v0 f (i,ti) =
   let accel = gravA `add3d` scale3d (1/m) f
       accel1 = scale3d (1/m) i
       limit (p@(x,y,z),v@(vx,vy,vz)) = if belowGround zoffs p then ((x,y,0),(vx,vy, if vz < 0 then 0 else vz)) else (p,v)
       d = t2d $ t1 - t0
       di = max 0 (min (t2d (ti - t0)) d)
   in limit (p0 `add3d` (scale3d d v0) `add3d` (scale3d (d^2 / 2) accel) `add3d` (scale3d (di^2/2) accel1), 
             v0 `add3d` (scale3d d accel) `add3d` (scale3d di accel1))

calcAccel t zoffs m p f (i,ti) = 
    let (x,y,z) = gravA `add3d` scale3d (1/m) f `add3d` (if ti < t then (0,0,0) else scale3d (1/m) i) in
        (x, y, if belowGround zoffs p then 0 else z)
  
-- i have no enthusiasm to work out if this is right... :(      
-- rotDyn t0 t1 t2d radius mass rot0 omega0@(ox0,oy0,oz0) torques = 
--     let inertia = momentOfIntertia mass radius
--         dt = t2d $ t1 - t0
--         accels = map ( \ ((tx,ty,tz),tf) -> ((tx/inertia,ty/inertia,tz/inertia), max 0 (min (t2d (tf - t0)) dt))) torques
--         lim theta = let rev = theta / (2 * pi)
--                         (_,frac) = properFraction rev in rev * 2 * pi
--         lim3 (x,y,z) = (lim x, lim y, lim z)
--         limscale scale v = lim3 $ scale3d scale v
--         rot = foldl ( \ r r1 -> r `quaternionMultiply` r1) rot0 $
--                 ((rotationsToQuaternion P123 $ limscale dt omega0) :
--                 (map (\ (a,d) -> rotationsToQuaternion P123 (limscale (d^2 * 0.5) a)) accels))
--         omega = foldl (\ o (a,d) -> lim3 $ o `add3d` (scale3d d a)) omega0 accels
--     in (rot, omega)
    
rotDyn dt radius mass rot0 omega0 torque =
    let inertia = momentOfInertia mass radius
        angularAcceleration = scale3d (1/inertia) torque
        domega = scale3d dt angularAcceleration
        drotV = (scale3d dt omega0) `add3d` (scale3d (dt^2/2) angularAcceleration)
        drot = let mag = mag3d drotV in if mag == 0 then (0,0,0,1) else axisAngleToRotation  (norm3d drotV) mag
    in (rot0 `quaternionMultiply` drot, omega0 `add3d` domega)

totalTorque t0 t1 t2d torques =
    let dt = t2d $ t1 - t0
        torques' = map (\ (torque,tf) -> scale3d ((max 0 (min (t2d (tf - t0)) dt)) / dt) torque) torques
    in foldl add3d (0,0,0) torques'
    
checkIntersections toBB cmp objects = concat (go objects)
    where go [] = []
          go (o:os) = [ pair o o' | (True,o') <- zip (map (bbIntersect (toBB o) . toBB ) os) os] : (go os)
          pair o o' = if cmp o o' == LT then (o,o') else (o',o)
          
dampZForce tau zt z0 m vz0 f = m * ((2 * (p - tau * vz0) / (tau^2)) - (gravC + f * m)) where p = zt - z0

dampForce tau pt p0 m v0 f = 
    scale3d m ((scale3d (2/(tau^2)) (p `diff3d` (scale3d tau v0))) `diff3d` (gravA `add3d` (scale3d m f)))
    where p = pt `diff3d` p0

-- I of sphere
momentOfInertia mass r = (2/5) * mass * r^2

-- ignoring strength for now...
dampTorque tau strength rt r0 mass radius omega0 torque0 =
    let inertia = momentOfInertia mass radius
        r1 = let mag = mag3d omega0 in
                 if mag == 0 then r0 else r0 `quaternionMultiply` (axisAngleToRotation (norm3d omega0) (mag*tau))
        rotNeeded = ((invertQuaternion r1) `quaternionMultiply` rt)
        (axis,angle) = axisAngleFromRotation rotNeeded
        -- angle = tau^2 * ((1/inertia*2) * (torque + torque0))
    in scale3d (angle * 2 * inertia / (tau^2)) axis `diff3d` torque0
-- testing...
iterativeDampingZ m z v zt tau dt = let f = dampZForce tau zt z m v 0
                                        z' = z + v * dt
                                        v' = v + (f/m) * dt in
    ((z',v',f) : iterativeDampingZ m z' v' zt tau dt)

iterativeDamping m p v pt tau dt = let f = dampForce tau pt p m v (0,0,0)
                                       p' = p `add3d` (scale3d dt v)
                                       v' = v `add3d` (scale3d (dt/m) f) in
    ((p',v',f) : iterativeDamping m p' v' pt tau dt)
        
