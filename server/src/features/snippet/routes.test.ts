import { Pool } from 'pg'
import request from 'supertest'
import { createApp } from '../../index'
import { createDbPool, withConn } from '../../infrastructure/db'

afterAll(async done => {
   const pool = await createDbPool()
   await withConn(pool as Pool, conn => conn.query('TRUNCATE TABLE snippets'))()
   await withConn(pool as Pool, conn => conn.query('TRUNCATE TABLE users'))()
   done()
})

describe('Snippet routes test suite', () => {
   it('Test POST /snippet with correct request body', async () => {
      const app = await createApp()

      const { body, status } = await request(app).get('/template/5,6')

      expect(status).toBe(200)
      expect(body.templates).toBeDefined()
   })
})
