_qGetSum :: -- x herein is either Node or LNode Expr
     (RSLT -> Node -> x) -- | gets what's there; used for QAt.
  -- Can safely be unsafe, because the QAt's contents are surely present.
  -> (RSLT -> [x])       -- | nodes or labNodes; used for QLeaf
  -> (RSLT -> RelSpec -> Either DwtErrSum [x])
    -- | matchRel or matchRelLab; used for QRel
  -> RSLT -> QNode -> Either DwtErrSum [x]
_qGetSum f _ _ g (QAt n) = return $ if gelem n g then [f g n] else []
_qGetSum _ f _ g (QLeaf l) = return $ f $ labfilter (==l) $ dropEdges g
_qGetSum _ _ f g (QRel qt qms) = prependCaller "_qGet: " $ do
  t <- qGet1 g qt   -- TODO ? case of multiple qt, qms matches
  ms <- mapM (qGet1 g) qms
  let relspec = mkRelSpec t ms
  f g relspec

qGetSum :: RSLT -> QNode -> Either DwtErrSum [Node]
qGetSum = _qGet (\_ n -> n) nodes matchRel

qGetLabSum :: RSLT -> QNode -> Either DwtErrSum [LNode Expr]
qGetLabSum = _qGet f labNodes matchRelLab where
  f g n = (n, Mb.fromJust $ lab g n)

qGet1Sum :: RSLT -> QNode -> Either DwtErrSum Node
qGet1Sum g q = prependCaller "qGet1: " $ case qGet g q of
    Right [] -> Left (FoundNo, queryError, ".")
    Right [a] -> Right a
    Right as -> Left (FoundMany, queryError, ".")
    Left e -> Left e
  where queryError = mQNode .~ Just q $ noErrOpts 

qPutStSum :: QNode -> StateT RSLT (Either DwtErrSum) Node
qPutStSum (QRel qt qms) = do
  -- TODO ? would be more efficient to return even the half-completed state
  let tag = prependCaller "qPutSt: " -- TODO: use
  t <- qPutSt qt
  ms <- mapM qPutSt qms
  g <- get
  let matches = matchRel g $ mkRelSpec t ms
  insRelSt t ms
qPutSt (QAt n) = lift $ Right n
qPutSt q@(QLeaf x) = get >>= \g -> case qGet1 g q of
  Right n -> lift $ Right n
  Left (FoundNo,_,_) -> let g' = insLeaf x g
    in put g' >> lift (Right $ maxNode g')
  Left e -> lift $ prependCaller "qPutSt: " $ Left e

