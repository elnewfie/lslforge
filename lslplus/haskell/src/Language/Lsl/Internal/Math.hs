module Language.Lsl.Internal.Math(
    dist3d2,
    dist3d,
    mag3d2,
    mag3d,
    dot3d,
    norm3d,
    diff3d,
    add3d,
    neg3d,
    scale3d,
    rot3d,
    Permutation3(..),
    quaternionToMatrix,
    matrixToQuaternion,
    quaternionMultiply,
    invertQuaternion,
    quaternionToRotations,
    rotationsToQuaternion,
    cross,
    rotationBetween,
    angleBetween,
    axisAngleToRotation,
    axisAngleFromRotation) where

-- import Debug.Trace
dist3d2 (x1,y1,z1) (x2,y2,z2) = ((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
dist3d v0 v1 = sqrt $ dist3d2 v0 v1

dot3d (x1,y1,z1) (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

mag3d2 (x,y,z) = (x^2 + y^2 + z^2)
mag3d v = sqrt $ mag3d2 v

norm3d v@(x,y,z) = let mag = mag3d v in (x/mag,y/mag,z/mag)

diff3d (x,y,z) (x',y',z') = (x - x', y - y', z - z')
add3d (x,y,z) (x',y',z') = (x + x', y + y', z + z')
neg3d (x,y,z) = (-x,-y,-z)

scale3d v (x,y,z) = (v*x,v*y,v*z)

quaternionToMatrix (x,y,z,s) =
    ((s*s + x*x - y*y - z*z, 2*x*y - 2*s*z,         2*x*z + 2*s*y),
     (2*x*y + 2*s*z,         s*s - x*x + y*y - z*z, 2*y*z - 2*s*x),
     (2*x*z - 2*s*y,         2*y*z + 2*s*x,         s*s - x*x - y*y + z*z))
     
-- reversing the conversion:
matrixToQuaternion ((r00,r01,r02),(r10,r11,r12),(r20,r21,r22)) =
    (sign (r21 - r12) * (sqrt (max 0 (1 + r00 - r11 - r22 ))) / 2,
     sign (r02 - r20) * (sqrt (max 0 (1 - r00 + r11 - r22))) / 2,
     sign (r10 - r01) * (sqrt (max 0 (1 - r00 - r11 + r22 ))) / 2,
     (sqrt (max 0  (1 + r00 + r11 + r22))) / 2)

quaternionMultiply (x1,y1,z1,s1) (x2,y2,z2,s2) =
    ((s1 * x2 + x1 * s2 + y1 * z2 - z1 * y2),
     (s1 * y2 - x1 * z2 + y1 * s2 + z1 * x2),
     (s1 * z2 + x1 * y2 - y1 * x2 + z1 * s2),
     (s1 * s2 - x1*x2 - y1 * y2 - z1 * z2))

rot3d vec rotation =
    (quaternionToMatrix rotation) `matMulVec` vec
    where matMulVec ((a1,b1,c1),(a2,b2,c2),(a3,b3,c3)) (a,b,c) =
              ((a1 * a + b1 * b + c1 * c),(a2 * a + b2 * b + c2 * c),(a3 * a + b3 * b + c3 * c))
    
invertQuaternion (x,y,z,s) = (-x,-y,-z,s)

rotationsToQuaternion :: RealFloat a => Permutation3 -> (a,a,a) -> (a,a,a,a)
rotationsToQuaternion order (x,y,z) = 
    let rx = (sin (x/2), 0.0, 0.0, cos (x/2))
        ry = (0.0, sin (y/2), 0.0, cos (y/2))
        rz = (0.0, 0.0, sin (z/2), cos (z/2))
        (r3,r2,r1) = permute order (rx,ry,rz)
    in ((r3 `quaternionMultiply` r2) `quaternionMultiply` r1)
    
data Permutation3 = P123 | P132 | P213 | P231 | P312 | P321

permute P123 (x,y,z) = (x,y,z)
permute P132 (x,y,z) = (x,z,y)
permute P213 (x,y,z) = (y,x,z)
permute P231 (x,y,z) = (y,z,x)
permute P312 (x,y,z) = (z,x,y)
permute P321 (x,y,z) = (z,y,x)

quatPermute (x,y,z,w) p = let (p1,p2,p3) = permute p (x,y,z) in (p1,p2,p3,w)

sign f = if f < 0 then -1 else 1

quaternionToRotations :: RealFloat a => Permutation3 -> Bool -> (a,a,a,a) -> (a,a,a)
quaternionToRotations rotOrder lh quat=
    let (p1,p2,p3,p0) = quatPermute quat rotOrder
        mult = if lh then -1 else 1
        sinTheta2 = 2 * (p0 * p2 + mult * p1 * p3)
        (sinTheta2',singularity) = 
            if (abs sinTheta2 >= 0.9999999) then (sign sinTheta2,True) else (sinTheta2,False)
        theta2 = asin sinTheta2'
        theta3 = if singularity then atan2 (p3*p0 + p1*p2) (0.5 - p1*p1 - p3*p3)
                 else atan2 (2 * (p0 * p3 - mult * p1 * p2)) (1 - 2 * (p2 * p2 + p3 * p3))
        theta1 = if singularity then 0
                 else atan2 (2 * ( p0 * p1 - mult * p2 * p3)) (1 - 2 * (p1 * p1 + p2 * p2))
    in (theta1,theta2,theta3)
    
cross (x1,y1,z1) (x2,y2,z2) = ((y1 * z2 - z1 * y2),(z1 * x2 - x1 * z2),(x1 * y2 - y1 * x2))

rotationBetween v0 v1 =
    let (v0',v1') = (norm3d v0,norm3d v1)
        axis = v0' `cross` v1'
        angle = acos (v0' `dot3d` v1') 
    in axisAngleToRotation axis angle

angleBetween (aX,aY,aZ,aS) (bX,bY,bZ,bS) =
   2 * acos ((aX * bX + aY * bY + aZ * bZ + aS * bS) / sqrt ((aX^2 + aY^2 + aZ^2 + aS^2) * (bX^2 + bY^2 + bZ^2 + bS^2)))
   
axisAngleToRotation (x,y,z) angle =
    let w = cos (angle/2.0)
        sinVal = sin (angle/2.0)
        x' = x * sinVal
        y' = y * sinVal
        z' = z * sinVal
    in (x',y',z',w)

axisAngleFromRotation (x,y,z,s) = 
    let s' = max (-1) (min 1 s)
        angle = 2 * acos s'
        sinVal = sin (angle/2)
        x' = x / sinVal
        y' = y / sinVal
        z' = z / sinVal
    in if sinVal == 0 then ((1,0,0),0) else ((x',y',z'),angle)
