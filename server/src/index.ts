import cookieParser from "cookie-parser"
import cors from "cors"
import express, { Express } from "express"
import morgan from "morgan"
import { PORT } from "./infrastructure/constants"

const createApp = async (): Promise<Express> => {
   const app = express()
   app.use(morgan("dev"))
      .use(
         cors({
            credentials: true,
            origin: "http://localhost:4000",
         })
      )
      .use(express.json())
      .use(cookieParser())
   return app
}

createApp().then(app => app.listen(PORT, () => console.log("listening on 4000")))
