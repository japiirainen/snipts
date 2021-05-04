import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import { Request, Response } from 'express'

import { processError } from '../../infrastructure/error'
import { allSnippets, newSnippet, snippetsByAuthor } from './snippetService'

export const snippetRoutes = {
   all(req: Request, res: Response): void {
      allSnippets(req.env)().then(e =>
         pipe(
            e,
            E.fold(processError(res), snippets => res.json({ snippets }))
         )
      )
   },

   new(req: Request, res: Response): void {
      newSnippet(req.env, req.body)().then(e =>
         pipe(
            e,
            E.fold(processError(res), snippet => res.json({ snippet }))
         )
      )
   },

   allByauthor(req: Request, res: Response): void {
      snippetsByAuthor(req.env, req.body)().then(e =>
         pipe(
            e,
            E.fold(processError(res), snippets => res.json({ snippets }))
         )
      )
   },
}
