import * as WA from '../../../assets/pkg/client';
type Elevate<Obj> = {
  [Key in keyof Obj as Exclude<Key, 'free'>]?: IsArray<Obj[Key]>;
};

type IsArray<Obj> =
  Obj extends Array<infer R> ? Array<Elevate<R>> : Elevate<Obj>;

export type Korisnik = Elevate<WA.Korisnik>;
export type Fakultet = Elevate<WA.Fakultet>;
export type Katedra = Elevate<WA.Katedra>;
export type StudentKolegij = Elevate<WA.StudentKolegij>;
export type Kolegij = Elevate<WA.Kolegij>;
export type Sekcija = Elevate<WA.Sekcija>;
export type Sadrzaj = Elevate<WA.Sadrzaj>;
export type Dodatno = Elevate<WA.Dodatno>;
export type Dokument = Elevate<WA.Dokument>;
