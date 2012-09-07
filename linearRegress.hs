-- Compute hypothesis for linear regression using data
-- i.e. compute a & b in y = h(x) = a + bx

-- newtype Variables = (Double, Double)
import Control.Applicative

type Alpha = Double

type Data =  [(Double, Double)]

-- a & b
type Theta = (Double, Double)

-- Start with values 0, 1 
-- Continue to get minimize difference until ()
-- 
computeHypothesis :: Data -> Alpha -> Theta
computeHypothesis d a = snd (minimizeDiff d a (0,1) 100)

minimizeDiff ::  Data -> Alpha -> Theta -> Double -> (Double, Theta)
minimizeDiff d a t pd = if current_diff < pd
                        then minimizeDiff d a new_t current_diff
                        else (current_diff, t)

            where   new_t = updateTheta d a t
                    current_diff = (sum (map ( ^ 2) (getDifference d t) ))  / (2.0 * fromIntegral (length d))


updateTheta :: Data -> Alpha -> Theta -> Theta
updateTheta d a t = (t1,t2)
            where   t1 = fst t - a * sum (getDifference d t) / fromIntegral (length d)
                    t2 = snd t - a * sum (map (\s -> head (getDifference [s] t) * fst s) d ) / fromIntegral (length d)


getDifference :: Data -> Theta -> [Double]
getDifference [] _ = []
getDifference d@(z:zs) t = diff : getDifference zs t 
        where   diff = (hypothesis t x - y)
                x = fst z
                y = snd z

hypothesis :: Theta -> Double -> Double
hypothesis ab x = fst ab + snd ab * x

