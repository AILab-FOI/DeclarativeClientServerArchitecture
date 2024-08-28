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
import { dohvati_korisnika } from '../../../assets/pkg/client';

@Component({
  selector: 'app-profile',
  standalone: true,
  imports: [CourseCardComponent],
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
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((user: Korisnik) => {
      if (user) {
        console.log(user);
        // TODO: UREĐIVANJE SEKCIJE QUERY
      }
      ref.close();
    });
  }
}
