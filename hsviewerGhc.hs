{-# LANGUAGE UndecidableInstances,OverlappingInstances,FlexibleInstances,FlexibleContexts #-}
{-# Language OverloadedStrings,TemplateHaskell#-}
{-# LANGUAGE ScopedTypeVariables,FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds,TypeFamilies,MultiParamTypeClasses,DeriveGeneric#-}

module Main where

import qualified Language.Haskell.GHC.ExactPrint.Parsers as E
import qualified Language.Haskell.GHC.ExactPrint.Print as E
import qualified Language.Haskell.GHC.ExactPrint.Utils as E
import HscTypes
import qualified Parser
import StringBuffer
import Lexer
import FastString
import SrcLoc
import DynFlags

import GHC
import  GHC.Paths as GP( libdir )
import GHC.SYB.Utils
import HsSyn
import OccName
import Outputable
import Data.Monoid
import GHC.Generics
import qualified Control.Lens as L
import Control.Monad
import Control.Monad.IO.Class
import DynFlags
import Data.Generics (Data, Typeable, mkQ, mkT, everything, everywhere)

import System.Directory
import System.IO.Unsafe
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Safe
import Data.List
import System.Directory
import System.Process
import System.IO
import System.Environment
import Debug.Trace

import Data.Maybe

import Data.Aeson
import Web.Scotty as Sc
import Data.Typeable
import Data.Tuple.All

instance (Outputable a)=>Show a where
  show =showSDocUnsafe.ppr
instance Show SDoc where
  show=showSDocUnsafe
instance Eq HsDeclu where
  (==)=(\x y->(show x)==(show y) )

type Sigu=Sig RdrName
type Hsbindu=HsBind RdrName
type Hsbindul=(Hsbindu,SrcCols)
type SrcCols=(Int,Int)--start col,end col
type TypedFun=(Maybe HsDeclu,HsDeclu)


--makeLenses ''HsDecl
type PrettyStr=String
type Expbody=PrettyStr
type Expname=PrettyStr
type Exptype=PrettyStr
--type Funcexp=(Expname,Expbody)

type FuncWtype=(Expname,Maybe Exptype,Expbody)

type Uid=Int
type Str=String
type Dat=Str
type Iden=Str
type Force=Int
data Hnode=Hnode Uid Iden Dat
  deriving (Show)
--  deriving (Show)
instance ToJSON Hnode where
  toJSON (Hnode id iden dat )=object ["id".=id,"name".=iden,"data".=dat,"Dir".=(6::Int)]

data Hedge=Hedge Dat Uid Uid Force 
  deriving (Show)
instance ToJSON Hedge where
  toJSON (Hedge dat src tg force )=object ["name".=dat,"source".=src,"target".=tg,"Dir".=(2::Int),"force".=force]

data Hgraph=Hgraph [Hnode] [Hedge]
  deriving (Show,Generic)

instance ToJSON Hgraph where
  toJSON (Hgraph a b)=object ["nodes".=toJSON a,"edges".=toJSON b]

type HsDeclu=HsDecl RdrName
ghcparse2 fp = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just GP.libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget fp Nothing
        setTargets [target]
        load LoadAllTargets
        modGraph <- depanal [] True
        case find ((== fp) . msHsFilePath) modGraph of
          Just modSummary -> do
            --modSum <- getModSummary $ mkModuleName $ "Database.Graph.HGraphStorage.API"
            presult1 <- parseModule modSummary
            return $ let ParsedModule _ presult _ _ =presult1 in trace ("ghc parse succeed") presult 

          _->trace ("ghc parse2 error::mod sum error") $ error ""

ghcparse::String-> IO (Located (HsModule RdrName))
ghcparse str =do
  runghc $ do
    hsc_env <- getSession
    let dflags = (hsc_dflags hsc_env) { verbosity = 99 }
    dyn<-getSessionDynFlags
    setSessionDynFlags dflags
    presult<-liftIO $ doParse str dflags
    case presult of
      POk _ r->do
        --liftIO $ putStrLn $ showData Parser 2 r
--        liftIO $ (print.showSDocUnsafe.ppr) r
        return $ r
      PFailed ss msg -> return $ trace (mconcat ["ghc parse error,",show ss,",,",show msg]) $ error ""
      where
        runghc x=defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
          runGhc (Just GP.libdir) $ x
        doParse str dflags = 
          return $ unP Parser.parseModule (mkPState
                                           dflags
                                           (stringToStringBuffer str)
                                           (mkRealSrcLoc (mkFastString "<interactive>") 1 1))


--      POk _ (SrcLoc.Located r)->liftIO $ print $ O.showSDoc dyn $ O.ppr r --putStrLn $ showData Parser 2 r
      --    Located (HsModule RdrName)
      -- file:///usr/local/haskell/ghc-7.10.2-x86_64/share/doc/ghc/html/libraries/ghc-7.10.2/SrcLoc.html

--past doc=showSDocDebug unsafeGlobalDynFlags doc
main=do
  putStrLn "haskell code analysing..."
  a:args<-getArgs
  hf<-readFile a
  hsmod<-ghcparse2 a
  mapM_ (putStrLn.show)  $  hsmodule2hsdecls $ unLoc   hsmod
  let (ts,fs,os)=declsPartition $  hsmodule2hsdecls $ unLoc   hsmod
--  putStrLn $ show $ (filter (\(_,_,z)->z/=0).genCiteNum.snd) $ matchTwithF ts fs
  let graph'=genGraph $ (\(x,y)->let cin=nubBy (\(x1,x2,x3) (y1,y2,y3)->(x1==y1)&&(x2==y2)) $ genCiteNum y in cin) $ matchTwithF ts fs
  let graph=
        let Hgraph ns es=graph'
        in Hgraph (ns++(let oth=genOthNodes os in oth)) es
--  print graph

{-}  putStrLn $ case unsafePerformIO $ E.parseModule a of
    Left _->"ghc parse error"
    Right (_,src)-> E.showGhc src
  -}
  let serverhome="/home/dw700/Desktop/btsync/workBS/haskell/hsviewer"  
  scotty 3000 $ do
    get "/:afile" $ do
      f<-param "afile"
      liftIO $ print f
      case f of
        "json"->do
          --liftIO $ mapM_ (putStrLn.showSDocUnsafe.ppr) $ str2decls hf
          Sc.json $ graph
          --return ()
        ""->file $ serverhome ++"/index.html"
        _->file $ serverhome++f--  mapM_ (\(x,y)->putStrLn $ "name:"++x++"body:"++y )$ intersperse ("","---\n") $ hs2hsdecls st
{-}
str2decls::String->[HsDecl RdrName]
str2decls str=ast
  where
    ast=case unsafePerformIO $ E.parseModule str of
      Left (srcspan,str)->trace (mconcat ["ghc parse error",show srcspan,show str]) $ error "ghc parse error111"
      Right (anns,src)-> fmap unLoc $  hsmodDecls $ unLoc src
-}
--gfoldover::Data a=> u->(a->a)->(a->)
gfoldover unit extractor binOp= everything binOp (mkQ unit extractor)

hsmodule2hsdecls::HsModule RdrName->[HsDecl RdrName]
hsmodule2hsdecls (HsModule _ _ _ m _ _)=fmap unLoc m

hsdecl2names::HsDeclu->[String]
hsdecl2names =nub . everything
  (++)
  (mkQ [] ((\x->[x]).occNameString))
  where
    mat (HsString s _)=s
--    mat nm@(OccName {})=show nm
declsPartition :: [HsDeclu] -> ([HsDeclu], [HsDeclu], [HsDeclu])
                  --typesig,func,oth
declsPartition ds = (flt 0,flt 1,flt 2)
  where
    flt i=fmap fst $ filter ((==i).snd) $ fmap splt ds
    splt f@(SigD TypeSig{})=(f,0)
    splt f@(ValD FunBind{})=(f,1)
    splt x=(x,2)
--{-}
trace1 ss x=trace
  (mconcat ss)
  x
matchTwithF::[HsDeclu]->[HsDeclu]->([FuncWtype],[TypedFun])--type,func
matchTwithF  ts fs= (fmap showHs $ mat ,mat)
--trace (mconcat ["\n",show ts,"\n",show fs ,"\n\n",show mat]) $ fmap showHs $ mat 
  where
  showHs::(Maybe HsDeclu,HsDeclu)->FuncWtype
  showHs (Nothing,(ValD f@(FunBind id _ matgrp _ _ _)))=(pp $ head $  everything (++) (mkQ [] occNameString) id,
                                                       Nothing,
                                                       pp f)
  pp::Show a=>a->String
  pp=show
  mat::[(Maybe HsDeclu,HsDeclu)]--matched type,func 
  mat=fmap matt fs
  matt f@(ValD FunBind{}) =findf f $ fmap --(\(t@(SigD (TypeSig)))-> (if nm==name then Just t else Nothing,f))
    (\t->(if elem (head $ getNames f) (getNames t) then Just t else Nothing,f))
    ts
  getNames ::Data a=>a->[String]
  getNames x =gfoldover [] ((\x->[x]).occNameString) (++) x
  findf f=findJustDef (Nothing,f) ((/=Nothing).fst)
--}
getFunName::HsDeclu->String
getFunName (ValD (FunBind id _ _ _ _ _))=head $ gfoldover [] ((\x->[x]).occNameString) (++) id

genCiteNum::[TypedFun]->[(TypedFun,TypedFun,Int)]
genCiteNum  defs= join $ fmap
  (\ d1@(_, fb1) ->
    (fmap
      (\d2@(_,fb2) -> (d1, d2, callsNum (getFunName fb2) fb1 ))
      $ defs\\[d1]))
  defs
  where
    callsNum::String->HsDeclu->Int
    callsNum s hdecl=if elem s (hsdecl2names hdecl) then 1 else 0 --trace (show $ hsdecl2names hdecl) $ 
typedfun2funcwtype::TypedFun->FuncWtype
typedfun2funcwtype (mbtp,fun)=(getFunName fun,maybe Nothing ((Just).show) mbtp,show fun)

genGraph::[(TypedFun,TypedFun,Int)]->Hgraph
genGraph xs=Hgraph nodesi edges
  where
    nodesi::[Hnode]
    nodesi=zipWith
      (\((tp,fun),_,_) i->Hnode i (getFunName fun) ((maybe "" (show) tp)++"\n"++(show fun)))
      (nubBy (\x y->(sel1 x)==(sel1 )y) xs)
      [0..]
    edges::[Hedge]
    edges=fmap mmp $ preedges $ fmap (\(tf1,tf2,i)->(typedfun2funcwtype tf1,typedfun2funcwtype tf2,i)) xs
    mmp ((n1,t1,b1),(n2,t2,b2),force)=Hedge
      "calls"
      (mat n1)
      (mat n2)
      force
    mat x=( (\(Hnode i _ _ )->i) $ head $ filter (\(Hnode _ nm  _)->nm==x) nodesi)
    preedges ssss=(\xss->filter ((/=0).(L.view L._3)) xss) ssss --list of 3 tuple 
    --funcwtypes=(uncurry matchTwithF).declsPartition.str2decls
genOthNodes :: [HsDeclu] -> [Hnode]
genOthNodes decs = [Hnode (-1) "" $ pp]
  where
        pp = case decs of
                [] -> []
                decss -> foldl1 (\ d1 d2 -> mconcat [d1, "\n", d2]) $
                           fmap show decss
