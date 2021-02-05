import dotenv from 'dotenv'
import path from 'path'

dotenv.config({
   path:
      process.env.NODE_ENV === 'production'
         ? path.join(__dirname, '..', '..', '.env')
         : path.join(__dirname, '..', '..', '.env.development'),
})

export const config = {
   port: process.env.PORT,
   db: {
      database: process.env.DB_NAME,
      user: process.env.DB_USER,
      password: process.env.DB_PASSWORD,
      host: process.env.DB_HOST,
      port: process.env.DB_PORT,
   },
}
