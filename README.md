# warp-tls-simple

Simplified TLS configuration for Warp servers with automatic certificate generation.

**Documentation**: [Hackage](https://hackage.haskell.org/package/warp-tls-simple)

## Features

- HTTPS by default with auto-generated certificates
- Zero-configuration development setup  
- Production-ready with custom certificate support
- CLI integration via `optparse-applicative`

## Quick Start

```haskell
import Network.Wai.Handler.WarpTLS.Simple
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = do
  let settings = Warp.defaultSettings & Warp.setPort 8080
  startWarpServer settings "./state" TLSAuto myWaiApplication
```

See [Hackage documentation](https://hackage.haskell.org/package/warp-tls-simple) for complete API reference.
