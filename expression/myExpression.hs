data MyExpression
  = XConst Double
  | XVar String
  | XOp1 String MyExpression
  | XOp2 String MyExpression MyExpression

xToString :: MyExpression -> String
xToString (XConst a) = show a
xToString (XVar a) = a
xToString (XOp1 o a) = "(" ++ o ++ (xToString a) ++ ")"
xToString (XOp2 o a b) = "(" ++ (xToString a) ++ o ++ (xToString b) ++ ")"