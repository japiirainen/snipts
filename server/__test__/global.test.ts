import { createDbPool } from '../src/infrastructure/db'
import { Pool } from 'pg'
import { insertUser } from '../src/features/auth/userRepo'

beforeAll(async () => {
   const pool = await createDbPool()
   await insertUser(
      { username: 'joona', email: 'joona.piirainen@gmail.com', password: 'foobar' },
      pool as Pool
   )()
}, 20000)

afterAll(async () => {
   const pool = (await createDbPool()) as Pool
   await Promise.all([
      pool.query('drop table if exists users'),
      pool.query('drop table if exists snippets'),
      pool.query('drop table if exists likes'),
      pool.query('drop table if exists comments'),
   ])
})

test('foo', () => expect(1 + 1).toBe(2))
