import dotenv from 'dotenv'
import path from 'path'

dotenv.config({
   path:
      process.env.NODE_ENV === 'production'
         ? path.join(__dirname, '..', '..', '.env')
         : process.env.NODE_ENV === 'testing'
         ? path.join(__dirname, '..', '..', '.env.test')
         : path.join(__dirname, '..', '..', '.env.development'),
})

export const config = {
   application: {
      port: process.env.PORT,
      name: process.env.NAME,
   },
   db: {
      database: process.env.DB_NAME,
      user: process.env.DB_USER,
      password: process.env.DB_PASSWORD,
      host: process.env.DB_HOST,
      port: process.env.DB_PORT,
   },
   jwt: {
      at: process.env.ACCESS_TOKEN_SECRET,
      rt: process.env.REFRESH_TOKEN_SECRET,
   },
}
