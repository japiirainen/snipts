import { CustomError } from 'ts-custom-error'
import { ApplicationError, InvalidRequest, ValidationFailed } from '../../infrastructure/error'
import * as I from 'io-ts'
import * as TE from 'fp-ts/TaskEither'
import { BcryptError, hashPassword } from '../../infrastructure/bcrypt'
import { pipe } from 'fp-ts/function'
import { Env } from '../../infrastructure/env'
import { mapLeft } from 'fp-ts/lib/Either'
import * as O from 'fp-ts/Option'
import { findUserByEmail, findUserByUsername, insertUser, InsertUserDTO } from './userRepo'
import { Pool } from 'pg'
import * as E from 'fp-ts/lib/Either'
import { DBError } from '../../infrastructure/db'

class UserAlreadyExists extends CustomError implements ApplicationError {
   status = 400
   code = 'UserAlreadyExists'
   log = true
}

const RegisterBody = I.interface({
   username: I.string,
   email: I.string,
   password: I.string,
})

type RegisterBodyT = I.TypeOf<typeof RegisterBody>

export const register = (
   env: Env,
   rawBody: unknown
): TE.TaskEither<DBError | ValidationFailed | InvalidRequest, void> => {
   return pipe(
      TE.fromEither(
         pipe(
            RegisterBody.decode(rawBody),
            mapLeft(_ => new InvalidRequest())
         )
      ),
      TE.chain(body =>
         pipe(
            validateBody(body),
            TE.fromOption(() => new ValidationFailed())
         )
      ),
      TE.chain(dto => tryInsertUser(dto, env.pool))
   )
}
const tryInsertUser = (
   dto: InsertUserDTO,
   pool: Pool
): TE.TaskEither<DBError | UserAlreadyExists | BcryptError, void> =>
   pipe(
      findUserByEmail(dto.email, pool),
      TE.alt(() => findUserByUsername(dto.password, pool)),
      TE.chain(maybeUser =>
         pipe(
            maybeUser,
            O.fold(
               () => TE.right(maybeUser),
               () => TE.left(new UserAlreadyExists())
            )
         )
      ),
      TE.chain(() => hashPassword(dto.password)),
      TE.chain(hashedPassword => insertUser({ ...dto, password: hashedPassword }, pool))
   )

const validateBody = (body: RegisterBodyT): O.Option<InsertUserDTO> =>
   pipe(
      O.of(body),
      O.filter(x => x.username.length >= 4),
      O.filter(x => x.username.length < 99),
      O.filter(x => x.email.includes('@')),
      O.filter(x => x.password.length >= 6),
      O.map(x => ({ username: x.username, password: x.password, email: x.email }))
   )
