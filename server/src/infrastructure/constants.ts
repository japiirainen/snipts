import dotenv from "dotenv"
import path from "path"
import * as t from "io-ts"
import { pipe } from "fp-ts/lib/pipeable"
import { fold } from "fp-ts/lib/Either"
import { identity } from "fp-ts/lib/function"

dotenv.config({
   path:
      process.env.NODE_ENV === "production"
         ? path.join(__dirname, "..", "..", ".env")
         : path.join(__dirname, "..", "..", ".env.development"),
})

export const PORT = pipe(
   t.string.decode(process.env.PORT),
   fold(() => "4000", identity)
)
