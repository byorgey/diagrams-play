{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
  #-}
-- Module for tree drawing.

module Tree where

import Data.Typeable
import Data.Tree
import Diagrams.Prelude

import Data.Default

-- Drawing rose trees

data RenderTreeOpts v =
  RenderTreeOpts
  { parentSep  :: Double
  , siblingSep :: Double
  , edgeStyle  :: Style v
  }

instance Default (RenderTreeOpts v) where
  def = RenderTreeOpts
        { parentSep  = 1
        , siblingSep = 1
        , edgeStyle  = mempty
        }

type Level = Int

data NodeLabel = Parent Level | Child Level Int
  deriving (Typeable, Eq, Ord, Show)

instance IsName NodeLabel

-- | @renderTree@ draws a rose tree using the given options and
--   function for rendering nodes.  The local origin of the whole tree
--   is the local origin of the root node.
renderTree :: (Backend b R2, Renderable (Path R2) b)
           => RenderTreeOpts R2        -- ^ Rendering options
           -> (a -> Diagram b R2)      -- ^ How to draw the nodes
           -> Tree a -> Diagram b R2   -- ^ Render a tree
renderTree = renderTree' 0 where
  renderTree' n opts f (Node a children)
    = f a # named (Parent n)
          # flip (beside unit_Y) (pSep === childrenD # centerX)
          # addEdges n childNames
    where
      childrenD = hcat' with {sep = siblingSep opts}
                . zipWith named childNames
                . map (renderTree' (n+1) opts f)
                $ children
      pSep | null children = mempty
           | otherwise     = strutY (parentSep opts)
      childNames = map (Child n) [0 .. length children - 1]
    
addEdges n childNames =
  withName (Parent n) $ \p ->
  withNames childNames $
  flip atop . mconcat . map (\c -> location p ~~ location c)
