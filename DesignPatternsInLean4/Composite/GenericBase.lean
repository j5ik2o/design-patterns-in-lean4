/-
  Composite Pattern - Generic-based Implementation
  Rust の generic bounds を使ったアプローチに相当。
  ディレクトリに格納できるエントリの型を制約できる。
-/

namespace Composite.GenericBase

/-- エントリの基本インターフェース -/
class Entry (α : Type) where
  getName : α → String
  getSize : α → Nat
  printLineWithPrefix : α → String → String

/-- ファイルを表す構造体 -/
structure File where
  name : String
  size : Nat
  deriving Repr, BEq

instance : Entry File where
  getName f := f.name
  getSize f := f.size
  printLineWithPrefix f prefix_ := s!"{prefix_}/{f.name} ({f.size})"

instance : ToString File where
  toString f := s!"{f.name} ({f.size})"

/-- 型パラメータ付きディレクトリ（特定の型のエントリのみ格納可能） -/
structure Directory (ε : Type) [Entry ε] where
  name : String
  entries : List ε := []
  deriving Repr

namespace Directory

variable {ε : Type} [Entry ε]

def add (dir : Directory ε) (entry : ε) : Directory ε :=
  { dir with entries := dir.entries ++ [entry] }

def getSize (dir : Directory ε) : Nat :=
  dir.entries.foldl (fun acc e => acc + Entry.getSize e) 0

def getName (dir : Directory ε) : String :=
  dir.name

partial def printLineWithPrefix (dir : Directory ε) (prefix_ : String) : String :=
  let header := s!"{prefix_}/{dir.name}"
  let newPrefix := s!"{prefix_}/{dir.name}"
  let children := dir.entries.map (fun e => Entry.printLineWithPrefix e newPrefix)
  String.intercalate "\n" (header :: children)

instance : Entry (Directory ε) where
  getName := getName
  getSize := getSize
  printLineWithPrefix := printLineWithPrefix

instance : ToString (Directory ε) where
  toString d := s!"{d.name} ({d.getSize})"

end Directory

/-- 任意のエントリを格納できる汎用エントリ型 -/
inductive AnyEntry where
  | file : File → AnyEntry
  | dir : String → List AnyEntry → AnyEntry
  deriving Repr

namespace AnyEntry

def getName : AnyEntry → String
  | file f => f.name
  | dir name _ => name

def getSize : AnyEntry → Nat
  | file f => f.size
  | dir _ entries => entries.foldl (fun acc e => acc + e.getSize) 0

partial def printLineWithPrefix (e : AnyEntry) (prefix_ : String) : String :=
  match e with
  | file f => Entry.printLineWithPrefix f prefix_
  | dir name entries =>
    let header := s!"{prefix_}/{name}"
    let newPrefix := s!"{prefix_}/{name}"
    let children := entries.map (fun child => child.printLineWithPrefix newPrefix)
    String.intercalate "\n" (header :: children)

instance : Entry AnyEntry where
  getName := getName
  getSize := getSize
  printLineWithPrefix := printLineWithPrefix

instance : ToString AnyEntry where
  toString e := s!"{e.getName} ({e.getSize})"

end AnyEntry

/-- ファイルのみを格納できるディレクトリ（型安全） -/
abbrev FileOnlyDirectory := Directory File

/-- 任意のエントリを格納できるディレクトリ -/
abbrev MixedDirectory := Directory AnyEntry

end Composite.GenericBase
