declare namespace Express {
   export interface Request {
      env: import('./src/infrastructure/env').Env
   }
}
