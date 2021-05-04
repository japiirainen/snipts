import { Id } from './id'
import { User } from './user'

export interface Snippet {
   id: Id<Snippet>
   title: string
   author: Id<User>
   description?: string
   content: string
   created_at: string
}

export interface InsertSnippetDTO {
   title: string
   description?: string
   author: Id<User>
   content: string
}
