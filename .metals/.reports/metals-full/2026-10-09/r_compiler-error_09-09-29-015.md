error id: E89042BEB265C027211D7DEFFAE350BD
file://<WORKSPACE>/tests.scala
### dotty.tools.dotc.ast.Trees$UnAssignedTypeException: type of TypeApply(Ident(List),List(Ident(TestCase))) is not assigned

occurred in the presentation compiler.



action parameters:
offset: 206
uri: file://<WORKSPACE>/tests.scala
text:
```scala
import Expr.*, Value.*, EvalError.*
object Tests:
    case class TestCase(
        name: String,
        expr: Expr,
        expected: Either[EvalError, Value]
    )

    private val tests = List[TestCase][@@]
```


presentation compiler configuration:
Scala version: 3.3.8-bin-nonbootstrapped
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.8/scala3-library_3-3.3.8.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.18/scala-library-2.13.18.jar [exists ]
Options:





#### Error stacktrace:

```
dotty.tools.dotc.ast.Trees$Tree.tpe(Trees.scala:74)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:210)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:104)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:523)
	scala.meta.internal.pc.CompilerAccess.withSharedCompiler(CompilerAccess.scala:149)
	scala.meta.internal.pc.CompilerAccess.withNonInterruptableCompiler$$anonfun$1(CompilerAccess.scala:133)
	scala.meta.internal.pc.CompilerAccess.onCompilerJobQueue$$anonfun$1(CompilerAccess.scala:210)
	scala.meta.internal.pc.CompilerJobQueue$Job.run(CompilerJobQueue.scala:153)
	java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1090)
	java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:614)
	java.base/java.lang.Thread.run(Thread.java:1527)
```
#### Short summary: 

dotty.tools.dotc.ast.Trees$UnAssignedTypeException: type of TypeApply(Ident(List),List(Ident(TestCase))) is not assigned