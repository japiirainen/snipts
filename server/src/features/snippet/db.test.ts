import { createDbPool } from '../../infrastructure/db'
import { allSnippets, findSnippetById, insertSnippet, findSnippetsByCreator } from './snippetRepo'
import { Pool } from 'pg'
import { pipe, identity } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import { insertUser } from '../auth/userRepo'

const testSnippet = {
   title: 'new snippet',
   description: 'how to sort an array',
   creator: 1,
   content: 'array.sort()',
}
const testUser = {
   username: 'joona',
   email: 'joona.piirainen@gmail.com',
   password: 'foobar',
}

beforeAll(async () => {
   const pool = await createDbPool()
   insertUser(testUser, pool as Pool)()
})

describe('snippets db test suite', () => {
   it('succesfully insert a new snippet', async () => {
      const pool = await createDbPool()
      const res = await insertSnippet(testSnippet, pool as Pool)().then(res =>
         pipe(
            res,
            E.fold(
               () => {
                  throw new Error('this should throw')
               },
               o =>
                  pipe(
                     o,
                     O.fold(() => {
                        throw new Error('this should also throw')
                     }, identity)
                  )
            )
         )
      )
      expect(res.content).toBe('array.sort()')
      expect(res.creator).toBe(testSnippet.creator.toString())
      expect(res.description).toBe('how to sort an array')
      expect(res.title).toBe('new snippet')
      expect(res.id).toBeDefined()
   })

   it('should return all snippets', async () => {
      const pool = await createDbPool()
      await insertSnippet(testSnippet, pool as Pool)()
      const res = await allSnippets(pool as Pool)().then(rows =>
         pipe(
            rows,
            E.fold(
               () => {
                  throw new Error('this should throw')
               },
               o =>
                  pipe(
                     o,
                     O.fold(() => {
                        throw new Error('this should also throw')
                     }, identity)
                  )
            )
         )
      )
      res.forEach(snip => {
         expect(snip.content).toBe('array.sort()')
         expect(snip.creator).toBe('1')
         expect(snip.description).toBe('how to sort an array')
         expect(snip.title).toBe('new snippet')
      })
   })

   it('find a snippet by its id', async () => {
      const pool = await createDbPool()
      const { id } = await insertSnippet(testSnippet, pool as Pool)().then(res =>
         pipe(
            res,
            E.fold(
               () => {
                  throw new Error('this should throw')
               },
               o =>
                  pipe(
                     o,
                     O.fold(() => {
                        throw new Error('this should also throw')
                     }, identity)
                  )
            )
         )
      )
      const snippet = await findSnippetById(id, pool as Pool)().then(res =>
         pipe(
            res,
            E.fold(
               () => {
                  throw new Error('this should throw')
               },
               o =>
                  pipe(
                     o,
                     O.fold(() => {
                        throw new Error('this should also throw')
                     }, identity)
                  )
            )
         )
      )

      expect(snippet.id).toBe(id)
      expect(snippet.content).toBe('array.sort()')
      expect(snippet.creator).toBe('1')
      expect(snippet.description).toBe('how to sort an array')
      expect(snippet.title).toBe('new snippet')
   })

   it('find all snippets by a creator', async () => {
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
                     O.fold(() => {
                        throw new Error('this should also throw')
                     }, identity)
                  )
            )
         )
      )

      const snippets = await findSnippetsByCreator(testSnippet.creator, pool as Pool)().then(res =>
         pipe(
            res,
            E.fold(
               () => {
                  throw new Error('this should throw')
               },
               o =>
                  pipe(
                     o,
                     O.fold(() => {
                        throw new Error('this should also throw')
                     }, identity)
                  )
            )
         )
      )
      snippets.forEach(s => {
         expect(s.content).toBe('array.sort()')
         expect(s.creator).toBe(testSnippet.creator.toString())
         expect(s.description).toBe('how to sort an array')
         expect(s.title).toBe('new snippet')
      })
   })
})
