import { Id } from '../../infrastructure/id'

export interface User {
   id: Id<User>
   username: string
   email: string
   password: string
   created_on: string
}

export interface PublicUser {
   id: Id<User>
   username: string
   createdOn: string
}

export const toPublicUser = (user: User): PublicUser => ({
   id: user.id,
   username: user.username,
   createdOn: user.created_on,
})
