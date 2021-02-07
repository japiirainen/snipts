import { Id } from '../../infrastructure/id'

export interface Snippet {
   id: Id<Snippet>
   title: string
   creator: string
   description?: string
   content: string
   created_at: string
}
