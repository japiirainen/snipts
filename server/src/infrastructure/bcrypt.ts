import { compare, hash } from 'bcrypt'
import * as TE from 'fp-ts/TaskEither'
import { CustomError } from 'ts-custom-error'
import { v4 as uuidv4 } from 'uuid'
import { ApplicationError } from './error'

class BcryptError extends CustomError implements ApplicationError {
   status = 500
   code = uuidv4()
   log = true
}

export const comparePasswords = (
   hashedPassword: string,
   attempt: string
): TE.TaskEither<BcryptError, boolean> =>
   TE.tryCatch(
      () => compare(attempt, hashedPassword),
      _ => new BcryptError('error while comparing passwords')
   )

export const hashPassword = (password: string): TE.TaskEither<BcryptError, string> =>
   TE.tryCatch(
      () => hash(password, 10),
      _ => new BcryptError('error while hashing password')
   )
