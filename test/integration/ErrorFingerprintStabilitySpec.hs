module ErrorFingerprintStabilitySpec (spec) where

import Data.Text qualified as T
import Pkg.ErrorFingerprint qualified as N
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)
import Utils (toXXHash)


runtimes :: [N.Runtime]
runtimes = [N.RGo, N.RNodejs, N.RWebjs, N.RPython, N.RJava, N.RPhp, N.RDotnet, N.RGeneric]


runtimeNames :: [Text]
runtimeNames = ["go", "Go", "GO", "nodejs", "NodeJS", "node", "node.js", "webjs", "WebJS", "web_js", "python", "Python", "python3", "java", "JAVA", "kotlin", "php", "dotnet", "DotNet", "dot_net", "ruby", "rust", "elixir", "dart", "", " go", "go ", "r_go", "RGo", "generic", "Generic"]


-- | (native runtime, trace)
traces :: [(N.Runtime, Text)]
traces =
  [ (N.RNodejs, "at processTicksAndRejections (node:internal/process/task_queues:95:5)\nat handleRequest (/app/src/server.js:42:15)")
  , (N.RNodejs, "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)")
  , (N.RNodejs, "at handler (/app/index.js:10:5)")
  , (N.RNodejs, "TypeError: Cannot read properties of undefined (reading 'id')\n    at handleRequest (/app/src/server.js:42:15)\n    at Layer.handle [as handle_request] (/app/node_modules/express/lib/router/layer.js:95:5)\n    at next (/app/node_modules/express/lib/router/route.js:149:13)\n    at processTicksAndRejections (node:internal/process/task_queues:95:5)")
  , (N.RNodejs, "    at async Promise.all (index 0)\n    at async UserService.getUser (/app/dist/services/user.service.mjs:10:3)")
  , (N.RNodejs, "at /app/src/anon.ts:5:7")
  , (N.RNodejs, "at <anonymous>")
  , (N.RNodejs, "at eval (eval at <anonymous> (/app/x.js:1:1), <anonymous>:1:1)")
  , (N.RNodejs, "at Object.<anonymous> (C:\\Users\\dev\\app\\index.js:10:5)\nat Module._compile (node:internal/modules/cjs/loader:1105:14)")
  , (N.RNodejs, "at new Foo (/app/foo.cjs:1:1)\nat Foo.bar.baz (/app/a.b.c.js:3)")
  , (N.RNodejs, "at foo(bar.js:1:2)\nat  (/app/x.js)\nat \nat ()")
  , (N.RWebjs, "at t.value (https://cdn.example.com/static/js/main.3f2a1b.chunk.js:2:123456)\nat e (https://cdn.example.com/static/js/vendor.js:1:999)\nat HTMLButtonElement.onclick (https://example.com/app.js:12:34)")
  , (N.RWebjs, "handleClick@https://example.com/static/app.js:10:20\n@debugger eval code:1:1\nhttps://example.com/app.js:1:1")
  , (N.RWebjs, "at r (webpack-internal:///./src/App.tsx:22:11)\nat async n (https://x.io/_next/static/chunks/pages/_app-0x1f.js:1:2)")
  , (N.RPython, "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper")
  , (N.RPython, "Traceback (most recent call last):\n  File \"/app/main.py\", line 10, in handler\n    result = process(x)\n  File \"/usr/lib/python3.11/json/decoder.py\", line 355, in raw_decode\n    raise JSONDecodeError(\"Expecting value\", s, err.value) from None\njson.decoder.JSONDecodeError: Expecting value: line 1 column 1 (char 0)")
  , (N.RPython, "File \"/app/venv/lib/python3.12/site-packages/django/core/handlers/base.py\", line 197, in _get_response\nFile \"/app/views.py\", line 3, in <lambda>\nFile \"/app/views.py\", line 4, in <listcomp>\nFile \"/app/views.py\", line 5, in <dictcomp>\nFile \"<frozen importlib._bootstrap>\", line 1, in _call")
  , (N.RPython, "File \"<stdin>\", line 1, in <module>\nFile \"C:\\\\app\\\\main.py\", line 3, in run\nFile \"/app/x.py\"\nFile \"/app/y.py\", line abc, in z")
  , (N.RPython, "File \"/usr/local/lib/python3.9/dist-packages/requests/api.py\", line 61, in request")
  , (N.RJava, "at com.example.MyClass.doWork(MyClass.java:25)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)")
  , (N.RJava, "at com.example.MyClass.doWork(MyClass.java:25)\nat com.example.Service.process(Service.java:50)")
  , (N.RJava, "java.lang.NullPointerException: msg\n\tat com.example.App.process(App.java:42)\n\tat java.base/java.lang.Thread.run(Thread.java:833)\nCaused by: java.io.IOException: x\n\tat sun.nio.ch.Net.connect(Native Method)\n\t... 12 more")
  , (N.RJava, "at com.example.Foo$bar$1.invokeSuspend(Foo.kt:20)\nat kotlinx.coroutines.DispatchedTask.run(DispatchedTask.kt:106)\nat jdk.internal.reflect.X.y(X.java:1)")
  , (N.RJava, "at com.example.Gen.<init>(Gen.java)\nat com.example.Gen.<clinit>(Unknown Source)\nat com.example.Gen.lambda$main$0(Native Method)\nat com.x.Y.<T>m(Y.java:1)\nat Main.main(Main.java:3)")
  , (N.RGo, "goroutine 1 [running]:\nmain.handleRequest(0xc0000b4000)\n\t/app/main.go:42 +0x1f\nmyapp/handler.ProcessData(0xc0000b2000)")
  , (N.RGo, "main.handleRequest(0xc0000b4000)\nruntime.goexit()\nnet/http.ListenAndServe(...)")
  , (N.RGo, "panic: runtime error: index out of range [5] with length 3\n\ngoroutine 18 [running]:\nmain.(*Server).handle(0xc000012345, {0x1, 0x2})\n\t/app/server.go:88 +0x1a5\nnet/http.HandlerFunc.ServeHTTP(0x0?, {0x7a3c40?, 0xc0001c2000?}, 0xc0001b0000?)\n\t/usr/local/go/src/net/http/server.go:2136 +0x29\ncreated by net/http.(*Server).Serve in goroutine 1\n\t/usr/local/go/src/net/http/server.go:3086 +0x5cb")
  , (N.RGo, "github.com/acme/app/internal/db.Query(...)\n\t/go/src/app/db.go:12 +0x2a\nsync.(*Once).doSlow(0x1)\nencoding/json.Unmarshal({0x1})\nfoo(0x1)\nnoparen")
  , (N.RPhp, "#0 /app/src/UserController.php(42): App\\UserController->getUser()\n#1 /app/vendor/slim/slim/Slim/Handlers/Strategies/RequestResponse.php(43): closure()")
  , (N.RPhp, "#0 /app/src/Handler.php(10): Handler->run()\n#1 /app/vendor/lib/Router.php(50): Router->dispatch()")
  , (N.RPhp, "#0 /app/src/Util.php(5): App\\Util::parse('x')\n#1 [internal function]: App\\Http\\{closure}(Object(Request))\n#2 /app/vendor/laravel/framework/src/Illuminate/Pipeline/Pipeline.php(180): Illuminate\\Pipeline\\Pipeline->Illuminate\\Pipeline\\{closure}()\n#3 {main}\n#4 phar:///app/tool.phar/src/a.php(1): x()")
  , (N.RPhp, "#\n#0\n#0 \n#12 /app/a.php: b()")
  , (N.RDotnet, "at MyApp.Controllers.UserController.GetUser(Int32 id) in /app/Controllers/UserController.cs:line 42\nat Microsoft.AspNetCore.Mvc.Infrastructure.ActionMethodExecutor.Execute(IActionResultTypeMapper mapper)")
  , (N.RDotnet, "at MyApp.Services.DbService.Query() in /app/Services/DbService.cs:line 15\nat System.Runtime.CompilerServices.TaskAwaiter.HandleNonSuccessAndDebuggerNotification(Task task)")
  , (N.RDotnet, "System.InvalidOperationException: x\n   at MyApp.Services.OrderService.PlaceOrder(Order order) in C:\\src\\MyApp\\Services\\OrderService.cs:line 87\n   at MyApp.Controllers.OrderController.Post(OrderDto dto) in /app/Controllers/OrderController.cs:line 21\n--- End of stack trace from previous location ---\n   at System.Runtime.ExceptionServices.ExceptionDispatchInfo.Throw()")
  , (N.RDotnet, "at MyApp.Repo`1.Get[T](Int32 id)\nat MyApp.Program.<Main>b__0_0()\nat MyApp.Svc.<DoAsync>d__5.MoveNext() in /app/Svc.cs:line 30\nat Newtonsoft.Json.JsonConvert.X()\nat Main")
  , (N.RGeneric, "some random frame info")
  , (N.RGeneric, "at Network.Wai.Handler.Warp.Run:123")
  , (N.RGeneric, "/app/app/models/user.rb:42:in `save!'\n/usr/lib/ruby/gems/3.2.0/gems/activerecord-7.0/lib/active_record/base.rb:10:in `block in transaction'")
  , (N.RGeneric, "thread 'main' panicked at src/main.rs:10:5:\nindex out of bounds\nstack backtrace:\n   0: rust_begin_unwind\n             at /rustc/abc/library/std/src/panicking.rs:645:5\n   1: myapp::handler::process\n             at ./src/handler.rs:22:9")
  , (N.RGeneric, "** (RuntimeError) boom\n    (my_app 0.1.0) lib/my_app/worker.ex:15: MyApp.Worker.run/1\n    (elixir 1.15.0) lib/enum.ex:1693: Enum.\"-map/2-lists^map/1-0-\"/2")
  , (N.RGeneric, "#0      MyWidget.build (package:my_app/widgets/my_widget.dart:42:7)\n#1      StatelessElement.build (package:flutter/src/widgets/framework.dart:5156:49)\n<asynchronous suspension>")
  , (N.RGeneric, "CallStack (from HasCallStack):\n  error, called at src/Foo.hs:10:5 in main:Foo\n...\n... 5 more\nworker 0xdeadbeef (foo)\na:b:c:d\n:1:x")
  , (N.RGeneric, "")
  , (N.RGeneric, "   \n\t\n  \r\n")
  , (N.RNodejs, "at 处理请求 (/app/源/服务器.js:1:2)\nFile \"/app/ünïcödé.py\", line 7, in fünc\nat Ü.ß<T>(Ä.java:1)\n💥 at 🔥 (/app/🔥.js:1:1)")
  , (N.RNodejs, "at a (/app/a.js:1:1)\r\nat b (/app/b.js:2:2)\r\n")
  , (N.RNodejs, T.intercalate "\n" $ replicate 50 "at dup (/app/dup.js:1:1)")
  ]


-- | Every trace line across runtimes, recombined deterministically into long and
-- mutated traces so every parser sees every other runtime's syntax.
generatedTraces :: [Text]
generatedTraces = take 300 $ unfoldr (Just . step) 12345
  where
    pool = concatMap (lines . snd) traces <> ["", "  ", "(", ")", ":", "at ", "#0 ", "File \"", " in ", ":line ", "0x1f", "<", "`1", "->", "::", ".go:", "goroutine"]
    poolN = length pool
    lcg s = (s * 1103515245 + 12345) `mod` 2147483648 :: Int
    pick s = fromMaybe "" $ listToMaybe $ drop (s `mod` poolN) pool
    step s0 =
      let n = 1 + s0 `mod` 40
          seeds = take (2 * n) $ drop 1 $ iterate lcg s0
          ln (a, b) = case b `mod` 4 of
            0 -> pick a <> pick b
            1 -> T.take (b `mod` 30) (pick a) <> T.drop (b `mod` 7) (pick b)
            _ -> pick a
          pairs = zip (everyOther seeds) (everyOther $ drop 1 seeds)
       in (T.intercalate (bool "\n" "\r\n" $ even s0) $ map ln pairs, lcg $ fromMaybe s0 $ viaNonEmpty last seeds)
    everyOther = \case (x : _ : xs) -> x : everyOther xs; [x] -> [x]; [] -> []


traceInputs :: [(N.Runtime, Text)]
traceInputs = [(r, t) | r <- runtimes, t <- map snd traces <> [T.intercalate "\n" $ concat $ replicate 10 $ map snd traces] <> generatedTraces]


messages :: [Text]
messages =
  [ ""
  , "   "
  , "msg"
  , "boom"
  , "Connection refused to 192.168.1.100:5432"
  , "Connection refused to 10.0.0.1:5432"
  , "User c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd not found"
  , "Error 404: Resource 12345 not available\n\nDetails here\nMore info"
  , "   Trimmed message   "
  , "line one\r\nline two\r\nline three"
  , "\n\n   \nthird line first\n\tfourth"
  , "connect ECONNREFUSED ::1:3000 and fe80::1%eth0 and 2001:db8::ff00:42:8329"
  , "user@example.com and first.last+tag@sub.example.co.uk not found"
  , "at 2024-01-15T10:30:00Z / 2024-01-15 10:30:00.123+00 / Oct 14, 2023 / 14-Oct-2023 / 1705314600 / 1705314600123"
  , "pointer 0xdeadbeef sha 3f2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a md5 d41d8cd98f00b204e9800998ecf8427e"
  , "token eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U expired"
  , "GET https://api.example.com/users/123?token=abc&page=2 failed with 503"
  , "Timeout after 30s (30000ms), value 3.14159 exceeded -42 by 1e10"
  , "Ошибка: пользователь 42 не найден — 用户 7 不存在"
  , "💥 failed 7 times"
  , "Warp: Client closed connection prematurely"
  , "broken pipe"
  , "Connection reset by peer"
  , "write EPIPE: premature EOF, client disconnected"
  , "cannot use broken pipe in filename"
  , "SB-3FDBA2D0A0A2 not processing"
  , "SB-90770E7BA9FD not processing"
  , "Apaylo request failed: {\"StatusCode\":400,\"Message\":\"No data found\"}"
  , "{\"error\":\"x\"}"
  , "htmx:error: The user aborted a request. {\"elt\":{\"listeners\":[{\"runCount\":7},{\"runCount\":9}]},\"pad\":\"" <> T.replicate 512 "x" <> "\"}"
  , "prefix {\"" <> T.replicate 511 "y"
  , "prefix {\"" <> T.replicate 510 "y"
  , T.replicate 200 "long message 12 "
  , "a\tb\tc  d"
  ]


exceptionTypes :: [Text]
exceptionTypes = ["", "  ", "TypeError", "  TypeError  ", "ValidationError", "InvalidRequest", "econnreset", "ECONNRESET", "IOError", "SocketException", "java.lang.NullPointerException", "System.IO.IOException", "Errno::EPIPE", "std::io::Error", "*net.OpError", "Error", "exception", "RuntimeError", "AbortError"]


type HashCase = (Text, Maybe Text, Maybe Text, N.Runtime, Text, Text, Text)


hashCases :: [HashCase]
hashCases =
  [ (p, svc, sn, r, typ, msg, st)
  | (i, (r, st)) <- zip [0 :: Int ..] traceInputs
  , let typ = cyc i exceptionTypes; msg = cyc (i * 7) messages
  , (p, svc, sn) <- bool [ctxs0] ctxs (i < 200)
  ]
    <> [(p, svc, sn, r, typ, msg, st) | typ <- exceptionTypes, msg <- messages, (r, st) <- [(N.RNodejs, ""), (N.RNodejs, "at handler (/app/index.js:10:5)"), (N.RGeneric, "at Network.Wai.Handler.Warp.Run:123")], (p, svc, sn) <- ctxs]
  where
    ctxs0 = ("proj1", Just "svc", Just "GET /a")
    ctxs = [ctxs0, ("proj1", Nothing, Nothing), ("p", Just "", Just ""), ("", Just "user-service", Just "/api/users"), ("proj2", Just "svc", Just "POST /b")]
    cyc i xs = fromMaybe "" $ listToMaybe $ drop (i `mod` length xs) xs


maskGroups :: [[Text]]
maskGroups =
  [ ["SB-3FDBA2D0A0A2 not processing", "SB-90770E7BA9FD not processing"]
  , ["a declined", "b declined"]
  , ["same message", "same message"]
  , ["SB-3FDBA2D0A0A2 not processing"]
  , []
  , ["order 1234 failed at step", "order 98 failed at step", "order 777777 failed at step"]
  , ["abcXdef", "abcYYdef"]
  , ["abcabc", "abcXabc", "abc"]
  , ["Payment pi_123abc declined: card", "Payment pi_9 declined: card"]
  , ["héllo 1 wörld", "héllo 22 wörld"]
  ]


masks :: [N.ErrorMask]
masks = [N.ErrorMask "SB-" " not processing" 12, N.ErrorMask "order " " failed at step" 2, N.ErrorMask "abc" "def" 1, N.ErrorMask "SB-" " not processing" 1]


render :: (Show a, Show b) => (a -> b) -> [a] -> [Text]
render f = map (\x -> show (x, f x))


-- | Every persisted-data path, rendered as "(input, output)" lines per category.
outputs :: [(Text, [Text])]
outputs =
  [ ("parseRuntime", render N.parseRuntime runtimeNames)
  , ("parseStackTrace", render (\(r, t) -> map (\f -> (f.filePath, f.moduleName, f.functionName, f.lineNumber, f.contextLine, f.isInApp)) $ N.parseStackTrace r t) traceInputs)
  , ("normalizeStackTrace", render (uncurry N.normalizeStackTrace) traceInputs)
  , ("normalizeMessage", render N.normalizeMessage (messages <> map snd traces))
  , ("isFrameworkTransportError", render (\(t, m) -> (N.isFrameworkTransportError t m, N.isFrameworkTransportError t (N.normalizeMessage m))) [(t, m) | t <- exceptionTypes, m <- messages])
  , ("computeErrorHashes", render (\(p, s, sn, r, t, m, st) -> let h = N.computeErrorHashes p s sn r t m st in (h.narrow, h.broad, h.shape, N.computeErrorFingerprint p s sn r t m st)) hashCases)
  , ("deriveErrorMask", render (fmap (\m -> (m.prefix, m.suffix, m.minVarLen)) . N.deriveErrorMask) maskGroups)
  , ("applyErrorMasks", render (\m -> (N.applyErrorMasks masks m, N.applyErrorMasks [] m)) (messages <> concat maskGroups <> ["SB-7 not processing", "order 1 failed at step"]))
  , ("shapeWithMasks", render (\(t, m) -> (N.shapeWithMasks masks "p" t m, N.shapeWithMasks [] "p" t m)) [(t, m) | t <- exceptionTypes, m <- messages <> concat maskGroups])
  ]


-- | Golden values computed by the pre-refactor (master) implementation. Fingerprints and
-- shapes are persisted, so any change here re-keys existing production issues.
expectedDigests :: [(Text, Int, Text)]
expectedDigests =
  [ ("parseRuntime", 31, "ff0d789f")
  , ("parseStackTrace", 2792, "9cd32400")
  , ("normalizeStackTrace", 2792, "19551ad2")
  , ("normalizeMessage", 82, "9e4ac89e")
  , ("isFrameworkTransportError", 646, "b0e4446e")
  , ("computeErrorHashes", 13282, "b96553d2")
  , ("deriveErrorMask", 10, "86fd5141")
  , ("applyErrorMasks", 55, "7a92d854")
  , ("shapeWithMasks", 1007, "707b6702")
  ]


-- | (narrow, broad, shape) per entry of 'traces' (type "TypeError", message "boom 42") …
expectedTraceHashes :: [(Text, Text, Text)]
expectedTraceHashes = [("d3a94b17", "d3a94b17", "0b3f8ff9"), ("d3a94b17", "d3a94b17", "0b3f8ff9"), ("c3500d04", "c3500d04", "0b3f8ff9"), ("d3a94b17", "d3a94b17", "0b3f8ff9"), ("23d64179", "23d64179", "0b3f8ff9"), ("a7019751", "a7019751", "0b3f8ff9"), ("addf9610", "addf9610", "0b3f8ff9"), ("16018088", "16018088", "0b3f8ff9"), ("49d3d87c", "49d3d87c", "0b3f8ff9"), ("4791d2a2", "4791d2a2", "0b3f8ff9"), ("c45ad0e6", "a42c3b3a", "0b3f8ff9"), ("3edee3a0", "3edee3a0", "0b3f8ff9"), ("c45ad0e6", "a42c3b3a", "0b3f8ff9"), ("875e29fe", "875e29fe", "0b3f8ff9"), ("c9c5d994", "c9c5d994", "0b3f8ff9"), ("3172098f", "3172098f", "0b3f8ff9"), ("e52fda34", "e52fda34", "0b3f8ff9"), ("a5d83447", "a5d83447", "0b3f8ff9"), ("96779db2", "96779db2", "0b3f8ff9"), ("0dd2bbba", "0dd2bbba", "0b3f8ff9"), ("de7b232d", "de7b232d", "0b3f8ff9"), ("18f02834", "18f02834", "0b3f8ff9"), ("9ee5fe71", "9ee5fe71", "0b3f8ff9"), ("4c5f87f7", "4c5f87f7", "0b3f8ff9"), ("7c561d79", "7c561d79", "0b3f8ff9"), ("b68a5b49", "b68a5b49", "0b3f8ff9"), ("70b769b9", "70b769b9", "0b3f8ff9"), ("0412d777", "0412d777", "0b3f8ff9"), ("2c87545c", "2c87545c", "0b3f8ff9"), ("2b63547e", "2b63547e", "0b3f8ff9"), ("29e6e1b6", "29e6e1b6", "0b3f8ff9"), ("5e79e948", "5e79e948", "0b3f8ff9"), ("ee2119cc", "ee2119cc", "0b3f8ff9"), ("293c342d", "293c342d", "0b3f8ff9"), ("04cdbc4c", "04cdbc4c", "0b3f8ff9"), ("a3716ae6", "a3716ae6", "0b3f8ff9"), ("57747f1e", "57747f1e", "0b3f8ff9"), ("20aaefbf", "20aaefbf", "0b3f8ff9"), ("04648983", "04648983", "0b3f8ff9"), ("78a4fb3c", "78a4fb3c", "0b3f8ff9"), ("c45ad0e6", "a42c3b3a", "0b3f8ff9"), ("c45ad0e6", "a42c3b3a", "0b3f8ff9"), ("f81e8175", "f81e8175", "0b3f8ff9"), ("c45ad0e6", "a42c3b3a", "0b3f8ff9"), ("c45ad0e6", "a42c3b3a", "0b3f8ff9"), ("f3d80bbe", "f3d80bbe", "0b3f8ff9"), ("2cfcf2da", "2cfcf2da", "0b3f8ff9"), ("567ede2e", "567ede2e", "0b3f8ff9")]


-- | … and per entry of 'messages' (no type, no stack).
expectedMessageHashes :: [(Text, Text, Text)]
expectedMessageHashes = [("cbf71181", "b194a3a7", "77797bc0"), ("cbf71181", "b194a3a7", "77797bc0"), ("12fbd552", "d2fa260e", "2a036a8f"), ("04471d56", "69842c75", "2689dd5b"), ("ee7af95c", "ee15d1f4", "4720a63f"), ("ee7af95c", "ee15d1f4", "4720a63f"), ("b8ffd50f", "d19e38f4", "379c3331"), ("302b765d", "2afa89d4", "0dd4f258"), ("e4335a5e", "8bc115cf", "7a4048b7"), ("8a08d73f", "6840419a", "21a363b8"), ("d73a07d1", "82cf46fa", "cadb7b84"), ("c5ab35e8", "7471485c", "2faab02e"), ("a2a1944e", "a881ae09", "aef76804"), ("87493091", "856e9415", "e17d9419"), ("a7971a3e", "32252e8c", "3ea371bf"), ("467324b3", "58c3cca5", "e01bda2e"), ("d9caf656", "7b43c7cd", "19d64d6c"), ("2ceb863d", "cdb65047", "204f785e"), ("db0a1637", "aaff5272", "9d1e71a9"), ("43ab667c", "e29df6db", "d51c5f0b"), ("43955b33", "43955b33", "e95c482e"), ("a19a4285", "a19a4285", "0fd96844"), ("f5974545", "f5974545", "9f799499"), ("5171b67b", "5171b67b", "8457e448"), ("3b4bed32", "3b4bed32", "271c3832"), ("3aa1fc74", "35829d1b", "960230db"), ("3aa1fc74", "35829d1b", "960230db"), ("244056f3", "a1f20cbf", "783014f3"), ("32e41395", "df839064", "0366f34a"), ("83cd337c", "4704610d", "08291c74"), ("7c3d8fca", "2b5f8595", "0eeac6e4"), ("9a9d24a5", "fb7089c4", "da295ca2"), ("a83d1ff2", "fbb510c0", "a6d3f31b"), ("d2d265f1", "8b40ae36", "1b7b4046")]


spec :: Spec
spec = describe "ErrorFingerprintStability" do
  it "fingerprints_stableAcrossRefactor" do
    let hashes r t m st = N.computeErrorHashes "proj1" (Just "svc") (Just "GET /a") r t m st & \h -> (h.narrow, h.broad, h.shape)
    [(name, length ls, toXXHash (T.unlines ls)) | (name, ls) <- outputs] `shouldBe` expectedDigests
    [(rt, hashes r "TypeError" "boom 42" t) | rt@(r, t) <- traces] `shouldBe` zip traces expectedTraceHashes
    [(m, hashes N.RGeneric "" m "") | m <- messages] `shouldBe` zip messages expectedMessageHashes
