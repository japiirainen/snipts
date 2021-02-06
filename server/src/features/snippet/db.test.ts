import { createDbPool, withConn } from '../../infrastructure/db'
import { insertSnippet } from './snippetRepo'
import { Pool } from 'pg'
import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'

const testSnippet = {
   title: 'new snippet',
   description: 'how to sort an array',
   content: 'array.sort()',
}

afterAll(async done => {
   const pool = await createDbPool()
   await withConn(pool as Pool, conn => conn.query('TRUNCATE TABLE snippets'))()
   done()
})

describe('snippets db test suite', () => {
   it('succesfully insert a new snippet', async () => {
      const pool = await createDbPool()
      await insertSnippet(testSnippet, pool as Pool)().then(res =>
         pipe(
            res,
            E.fold(
               () => {
                  throw new Error('this should throw')
               },
               o =>
                  pipe(
                     o,
                     O.fold(
                        () => {
                           throw new Error('this should also throw')
                        },
                        snippet => {
                           expect(snippet.id).toBeDefined()
                        }
                     )
                  )
            )
         )
      )
   })
})
