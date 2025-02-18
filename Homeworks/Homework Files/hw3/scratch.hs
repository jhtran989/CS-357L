testReal count = 
    let a = 1
    in
        test count a

test count a =
    if count > 5
    then 
        0
        else
            a + test (count + 1) a

-- >>> testReal 0
-- 6