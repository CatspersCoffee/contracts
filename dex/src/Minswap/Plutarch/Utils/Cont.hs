{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Minswap.Plutarch.Utils.Cont (pmatchC, pletC, ptraceC, ptraceIfFalseC, ptraceIfTrueC) where

import Plutarch.Prelude

pmatchC :: PMatch a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

ptraceC :: Term s PString -> TermCont s ()
ptraceC s = TermCont $ \f -> ptrace s (f ())

ptraceIfFalseC :: Term s PString -> TermCont @PBool s ()
ptraceIfFalseC s = TermCont $ \f -> ptraceIfFalse s (f ())

ptraceIfTrueC :: Term s PString -> TermCont @PBool s ()
ptraceIfTrueC s = TermCont $ \f -> ptraceIfTrue s (f ())
