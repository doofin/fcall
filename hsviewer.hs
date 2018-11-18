{-# Language OverloadedStrings,TemplateHaskell#-}
{-# LANGUAGE ScopedTypeVariables,FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds,TypeFamilies,MultiParamTypeClasses,DeriveGeneric#-}

module Main where
import Data.Data
import Safe
import qualified Language.Haskell.GHC.ExactPrint.Parsers as E
import qualified Language.Haskell.GHC.ExactPrint.Print as E
import Data.Monoid
import GHC.Generics
import System.Directory
import System.IO.Unsafe
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List
import System.Directory
import System.Process
import System.IO
import System.Environment
import Data.Maybe
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts
import Debug.Trace
import Data.Aeson
import Web.Scotty as Sc
import Data.Typeable
import qualified Control.Lens as L
import Control.Monad
import Control.Monad.IO.Class
--makeLenses ''HsDecl
type PrettyStr=String
type Expbody=PrettyStr
type Expname=PrettyStr
type Exptype=PrettyStr
--type Funcexp=(Expname,Expbody)

type FuncWtype=(Expname,Maybe Exptype,Expbody)

type Id=Int
type Str=String
type Dat=Str
type Iden=Str
type Force=Int
data Hnode=Hnode Id Iden Dat
  deriving (Show)
instance ToJSON Hnode where
  toJSON (Hnode id iden dat )=object ["id".=id,"name".=iden,"data".=dat,"Dir".=(6::Int)]

data Hedge=Hedge Dat Id Id Force 
  deriving (Show)
instance ToJSON Hedge where
  toJSON (Hedge dat src tg force )=object ["name".=dat,"source".=src,"target".=tg,"force".=force]
data Hgraph=Hgraph [Hnode] [Hedge]
  deriving (Show,Generic)
instance ToJSON Hgraph where
  toJSON (Hgraph a b)=object ["nodes".=toJSON a,"edges".=toJSON b]
         
--genCiteNum::[FuncWtype]->Hgraph
genOthNodes::[Decl]->[Hnode]
genOthNodes decs=[Hnode (-1) "" $ pp]
  where
    nodes=zipWith (\decl i->Hnode i "" $ prettyPrint decl) decs $ fmap (*(-1)) [1..]
    merge =foldl1 (\(Hnode i _ n1) (Hnode j _ n2)->Hnode i "" $ (n1)++(n1)) nodes
    pp=case decs of
      []->[]
      decss->foldl1 (\d1 d2->mconcat [d1,"\n",d2]) $ fmap prettyPrint decss
genGraph::[[(FuncWtype,FuncWtype,Int)]]->Hgraph
genGraph xs=Hgraph nodesi edges
  where
    nodes::[FuncWtype]
    nodes=nub $ fmap (L.view L._1) $ join xs
    nodesi::[Hnode]
    nodesi=zipWith (\(nm,tp,bod) i->Hnode i nm ((fromMaybe "" tp)++"\n"++bod)) nodes [0..]
    edges::[Hedge]
    edges=fmap mmp $ preedges xs
      where
        mmp ((n1,t1,b1),(n2,t2,b2),force)=Hedge
          "calls"
          (mat n1)
          (mat n2)
          force
          where
            mat x=( (\(Hnode i _ _ )->i) $ head $ filter (\(Hnode _ nm  _)->nm==x) nodesi)
    preedges ssss=(\xss->filter ((/=0).(L.view L._3)) $ join xss) ssss --list of 3 tuple 
    --funcwtypes=(uncurry matchTwithF).declsPartition.str2decls

--genCiteNum::[[(FuncWtype,FuncWtype,Int)]]    
genCiteNum defs=mat1
  where
--    fundefi=zipWith (\(a,b) id->(a,b,id)) defs [0..]
    mat1::[[(FuncWtype,FuncWtype,Int)]]
    mat1=fmap (\d1@(n1,t1,fb1)->
                (fmap (\d2@(n2,t2,fb2)->
                        (d1,d2,tappears n2 fb1))--how many does d2 call d1
                 (defs\\[d1])
                )
              ) defs--fun call of n1 in fb2

main::IO ()    
main=do
  putStrLn "input File name,with directory"
  a:args<-getArgs
  putStrLn $ case unsafePerformIO $ E.parseModule a of
    Left _->"ghc api parse error"
    Right (_,src)-> "ghc api parse ok"
  decls<-file2decls a
  let graph=decls2graph decls
  mapM_ print decls
    
  scotty 3000 $ do
    get "/:afile" $ do
      f<-param "afile"
      liftIO $ print f
      case f of
        "json"->do
          Sc.json $ graph
        ""->file $ "./index.html"
        _->file $ "./"++f--  mapM_ (\(x,y)->putStrLn $ "name:"++x++"body:"++y )$ intersperse ("","---\n") $ hs2hsdecls str
--  mapM_ (\(x,y)->putStrLn.fst)  $ hs2hsdecls str


tappear::T.Text->T.Text->Int
tappear s str=length $ T.breakOnAll s str

tappears::String->String->Int
tappears s str=length $ T.breakOnAll (T.pack s) (T.pack  str)

--withFstr::(String->String)->IO ()
withFstr f=do
  fs<-readFile "t.hs"
  putStrLn $ show $ encode $  f fs
--  mapM_ putStrLn $ fmap show $ f fs
decls2graph::[Decl]->Hgraph
decls2graph s =Hgraph (nodes1'++nodes2) $ trace (show funcwtypes) edges1
  where
    dp@(ts,fs,os)=declsPartition $  s --grph=(\xss->filter ((/=0).(L.view L._3)) $ join xss).genCiteNum.funcwtypes
    funcwtypes=matchTwithF ts fs
    nodes1'=fmap aaa nodes1
      where
        aaa (Hnode id iden ('\n':dat))=Hnode id iden dat
        aaa x=x
    Hgraph nodes1 edges1=genGraph $ genCiteNum  funcwtypes
    nodes2=genOthNodes os

str2graph::String->Hgraph
str2graph s =Hgraph (nodes1'++nodes2) $ trace (show funcwtypes) edges1
  where
    dp@(ts,fs,os)=declsPartition $ str2decls s --grph=(\xss->filter ((/=0).(L.view L._3)) $ join xss).genCiteNum.funcwtypes
    funcwtypes=matchTwithF ts fs
    nodes1'=fmap aaa nodes1
      where
        aaa (Hnode id iden ('\n':dat))=Hnode id iden dat
        aaa x=x
    Hgraph nodes1 edges1=genGraph$ genCiteNum  funcwtypes
    nodes2=genOthNodes os

{-tst=withFstr (graph)  
  where
    graph=genGraph.genCiteNum.funcwtypes
    grph=(\xss->filter ((/=0).(L.view L._3)) $ join xss).genCiteNum.funcwtypes
    funcwtypes=(uncurry matchTwithF).(\dcls->(L.view _1 $ declsPartition dcls ,L.view _2 $ declsPartition dcls)).str2decls-}
declsPartition::[Decl]->([Decl],[Decl],[Decl])--partition decls:  function def , type def,other
declsPartition  ds=(filter ((==0).snd.splt) ds,
             filter ((==1).snd.splt) ds,
             filter ((==2).snd.splt) ds) 
  where
    splt t@TypeSig {}=(t,0)
    splt p@(PatBind _ _ _ _)=(p,1)
    splt f@FunBind {}=(f,1)
    splt o=(o,2)
matchTwithF::[Decl]->[Decl]->[FuncWtype]--type,func
matchTwithF  ts fs=trace (mconcat ["\n",show ts,"\n",show fs ,"\n\n",show mat]) $ fmap extr $ mat 
  where
  extr::(Maybe Decl,Decl)->FuncWtype
  extr (Just t@(TypeSig _ ((Ident nm):_) _),f@(PatBind _ (PVar (Ident name)) _ _))=(name,Just $ pp t,pp f)
  extr (Just t@(TypeSig _ ((Ident nm):_) _),f@(FunBind ((Match _ (Ident name) _ _ _ _):_)))=(name,Just $ pp t,pp f)
  extr (Nothing,f@(PatBind _ (PVar (Ident name)) _ _))=(name,Nothing,pp f)
  extr (Nothing,f@(FunBind ((Match _ (Ident name) _ _ _ _):_)))=(name,Nothing,pp f)
  pp=prettyPrint

  mat::[(Maybe Decl,Decl)]--matched type,func algor completely wrong!!!
  mat=fmap matt fs
  matt f@(PatBind _ (PVar (Ident name)) _ _) =findJustDef (Nothing,f) ((/=Nothing).fst) $ fmap
    (\(t@(TypeSig _ ((Ident nm):_) _))-> (if nm==name then Just t else Nothing,f))
    ts
  matt f@(FunBind ((Match _ (Ident name) _ _ _ _):_)) = findJustDef (Nothing,f) ((/=Nothing).fst) $ fmap
    (\(t@(TypeSig _ ((Ident nm):_) _))-> (if nm==name then Just t else Nothing,f))
    ts
          
file2decls::FilePath->IO [Decl]
file2decls str=parseFile  str>>= \x->case x of
      ParseOk resok->let Module _ _ _ _ _ _ decls=resok
                     in return $ trace (show decls) decls
      ParseFailed loc str ->trace ("haskell-src-exts:fromParseResult: Parse failed at ["
                ++ srcFilename loc
                ++ "] ("
                ++ show (srcLine loc) ++ ":"
                ++ show (srcColumn loc) ++ "): " ++ str)
        return []

str2decls::String->[Decl]
str2decls str=res
  where
    --res=fromParseResult $ parseModule str
    res=case parseModule str of
      ParseOk resok->let Module _ _ _ _ _ _ decls=resok in decls
      ParseFailed loc str ->trace ("fromParseResult: Parse failed at ["
                ++ srcFilename loc
                ++ "] ("
                ++ show (srcLine loc) ++ ":"
                ++ show (srcColumn loc) ++ "): " ++ str) []
    
    
decls2idents::[Decl]->[(String,[String])]--funname,idens included in funbody
decls2idents xs=undefined
  where
    d2i (FunBind ((Match _ nm _ _ _ _):_))=()
decl2list::Decl->[Constr]
decl2list=gmapQ toConstr
