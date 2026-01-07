/-
  Composite Pattern Implementation in Lean4
  Based on: https://github.com/j5ik2o/design-patterns-in-rust/tree/main/src/composite

  Four approaches:
  1. InductiveBase - Rust's enum-based approach
  2. ClassBase     - Rust's trait-based approach
  3. SeparateBase  - Separate types for File and Directory
  4. GenericBase   - Generic constraints for type safety
-/

import DesignPatternsInLean4.Composite.InductiveBase
import DesignPatternsInLean4.Composite.ClassBase
import DesignPatternsInLean4.Composite.SeparateBase
import DesignPatternsInLean4.Composite.GenericBase

/-! ## Demo Functions -/

open Composite.InductiveBase in
def demoInductiveBase : IO Unit := do
  IO.println "=== Inductive-based Composite Pattern ==="
  -- ファイルを作成
  let file1 := Entry.file { name := "file1.txt", size := 100 }
  let file2 := Entry.file { name := "file2.txt", size := 200 }
  let file3 := Entry.file { name := "file3.txt", size := 300 }
  let file4 := Entry.file { name := "file4.txt", size := 400 }
  -- ディレクトリ構造を構築
  let subDir := Entry.directory "subdir" [file3, file4]
  let rootDir := Entry.directory "root" [file1, file2, subDir]
  IO.println s!"Root directory size: {rootDir.getSize}"
  IO.println ""
  IO.println "Directory structure:"
  IO.println rootDir.printLine

open Composite.ClassBase in
def demoClassBase : IO Unit := do
  IO.println "\n=== Class-based Composite Pattern ==="
  -- ファイルを作成
  let file1 := EntryWrapper.file { name := "document.pdf", size := 1024 }
  let file2 := EntryWrapper.file { name := "image.png", size := 2048 }
  let file3 := EntryWrapper.file { name := "data.csv", size := 512 }
  -- ディレクトリ構造を構築
  let subDir := EntryWrapper.directory "assets" [file2, file3]
  let rootDir := EntryWrapper.directory "project" [file1, subDir]
  IO.println s!"Project directory size: {rootDir.getSize}"
  IO.println ""
  IO.println "Directory structure:"
  IO.println rootDir.printLine

open Composite.SeparateBase in
def demoSeparateBase : IO Unit := do
  IO.println "\n=== Separate Types Composite Pattern ==="
  -- ファイルを作成
  let file1 : Entry := Entry.mkFile "readme.md" 50
  let file2 : Entry := Entry.mkFile "main.lean" 150
  let file3 : Entry := Entry.mkFile "test.lean" 100
  -- ディレクトリ構造を構築
  let srcDir : Entry := Entry.mkDir "src" [file2]
  let testDir : Entry := Entry.mkDir "tests" [file3]
  let rootDir : Entry := Entry.mkDir "myproject" [file1, srcDir, testDir]
  IO.println s!"Project size: {Entry.getSize rootDir}"
  IO.println ""
  IO.println "Directory structure:"
  IO.println (Entry.print rootDir)

open Composite.GenericBase in
def demoGenericBase : IO Unit := do
  IO.println "\n=== Generic-based Composite Pattern ==="
  -- ファイルのみのディレクトリ（型安全）
  let file1 : File := { name := "config.json", size := 256 }
  let file2 : File := { name := "data.bin", size := 1024 }
  let fileOnlyDir : FileOnlyDirectory := { name := "configs" }
  let fileOnlyDir := fileOnlyDir.add file1 |>.add file2
  IO.println s!"File-only directory: {fileOnlyDir}"
  IO.println s!"  Size: {fileOnlyDir.getSize}"
  -- 混合ディレクトリ
  let mixed1 := AnyEntry.file { name := "readme.txt", size := 100 }
  let mixed2 := AnyEntry.dir "subdir" [AnyEntry.file { name := "nested.txt", size := 50 }]
  let mixedDir := AnyEntry.dir "mixed" [mixed1, mixed2]
  IO.println s!"\nMixed directory size: {mixedDir.getSize}"
  IO.println "Structure:"
  IO.println (mixedDir.printLineWithPrefix "")

def compositeDemo : IO Unit := do
  demoInductiveBase
  demoClassBase
  demoSeparateBase
  demoGenericBase
