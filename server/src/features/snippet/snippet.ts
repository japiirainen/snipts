import { Id } from '../../infrastructure/id'
import { User } from '../auth/user'

export interface Snippet {
   id: Id<Snippet>
   title: string
   author: Id<User>
   description?: string
   content: string
   created_at: string
}
