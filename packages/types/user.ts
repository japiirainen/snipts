import * as i from 'io-ts'
import { Id } from './id'

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
   email: string
   createdOn: string
}

export interface InsertUserDTO {
   username: string
   email: string
   password: string
}

export const toPublicUser = (user: User): PublicUser => ({
   id: user.id,
   username: user.username,
   email: user.email,
   createdOn: user.created_on,
})

export const RegisterBody = i.type({
   username: i.string,
   email: i.string,
   password: i.string,
})

export type RegisterBodyT = i.TypeOf<typeof RegisterBody>

export const LoginBody = i.type({
   username: i.string,
   password: i.string,
})

export type LoginBodyT = i.TypeOf<typeof LoginBody>
