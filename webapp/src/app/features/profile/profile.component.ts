import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import { Korisnik, TokenService, UserService } from '../../core';
import {
  CourseCardComponent,
  openModal,
  UserEditComponent,
} from '../../shared';
import { Dialog } from '@angular/cdk/dialog';
import {
  dohvati_korisnika,
  uredi_djelatnika,
  uredi_studenta,
} from '../../../assets/pkg/client';
import { DepartmentCardComponent } from '../../shared/department-card/department-card.component';

@Component({
  selector: 'app-profile',
  standalone: true,
  imports: [CourseCardComponent, DepartmentCardComponent],
  templateUrl: './profile.component.html',
  styleUrl: './profile.component.scss',
})
export class ProfileComponent {
  public userService = inject(UserService);
  public tokenService = inject(TokenService);
  public user = signal<Korisnik>(this.userService.user());
  public id = input.required<string>();
  public isCurrenUser = computed(() => {
    return parseInt(this.id()) === this.userService.user().id;
  });
  public courses = computed(() => {
    console.log(this.user());
    return this.user().kolegiji;
  });

  private dialog = inject(Dialog);

  constructor() {
    effect(() => {
      if (!this.isCurrenUser()) {
        dohvati_korisnika(
          parseInt(this.id()),
          this.tokenService.accessToken(),
        ).then((res) => this.user.set(res));
      }
    });
  }
  openEdit(): void {
    let ref = openModal<Korisnik>(this.dialog, {
      data: structuredClone(this.userService.user()),
      tool: { view: UserEditComponent },
      inputs: {},
      title: 'Edit user',
    });
    ref.componentInstance['query'].subscribe((user: Korisnik) => {
      if (user) {
        if (user.uloga === 'Student') {
          uredi_studenta(
            user.id,
            user.opis,
            user.dodatno.nadimak,
            user.slika,
            this.tokenService.accessToken(),
          ).then((res) => {
            dohvati_korisnika(res, this.tokenService.accessToken()).then(
              (korisnik) => {
                this.user.set(korisnik);
                if (this.isCurrenUser()) {
                  this.userService.user.set(korisnik);
                }
              },
            );
          });
        } else {
          uredi_djelatnika(
            user.id,
            user.opis,
            user.dodatno.kabinet,
            user.slika,
            this.tokenService.accessToken(),
          ).then((res) => {
            dohvati_korisnika(res, this.tokenService.accessToken()).then(
              (korisnik) => {
                this.user.set(korisnik);
                if (this.isCurrenUser()) {
                  this.userService.user.set(korisnik);
                }
              },
            );
          });
        }
      }
      ref.close();
    });
  }
}
