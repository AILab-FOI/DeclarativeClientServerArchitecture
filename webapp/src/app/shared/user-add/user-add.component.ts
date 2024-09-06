import {
  Component,
  effect,
  inject,
  input,
  model,
  output,
  signal,
} from '@angular/core';
import { Korisnik, TokenService } from '../../core';
import { UserCardComponent } from '../user-card/user-card.component';
import { dohvati_korisnike_na_fakultetu } from '../../../assets/pkg/client';

@Component({
  selector: 'app-user-add',
  standalone: true,
  imports: [UserCardComponent],
  templateUrl: './user-add.component.html',
  styleUrl: './user-add.component.scss',
})
export class UserAddComponent {
  data = model.required<Korisnik>();
  token = inject(TokenService);
  fakultet = input.required<number>();
  korisnici = signal<Korisnik[]>([]);
  query = output<Korisnik>();
  selected = signal<number>(0);
  constructor() {
    effect(() => {
      dohvati_korisnike_na_fakultetu(
        this.fakultet(),
        this.token.accessToken(),
      ).then((res) => {
        this.korisnici.set(res);
      });
    });
  }

  select(korisnik: Korisnik) {
    this.data.set(korisnik);
    this.selected.set(korisnik.id);
    this.query.emit(this.data());
  }
}
