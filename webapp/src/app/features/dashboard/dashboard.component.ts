import { Component, computed, effect, inject, signal } from '@angular/core';
import { Kolegij, Korisnik, TokenService, UserService } from '../../core';
import { CourseCardComponent } from '../../shared/course-card/course-card.component';
import { dohvati_korisnika } from '../../../assets/pkg/client';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [CourseCardComponent],
  templateUrl: './dashboard.component.html',
  styleUrl: './dashboard.component.scss',
})
export class DashboardComponent {
  public user = inject(UserService);
  public token = inject(TokenService);
  public username = computed(() => {
    return this.user.user().ime + ' ' + this.user.user().prezime;
  });
  public courses = signal<Kolegij[]>([]);
  constructor() {
    effect(() => {
      dohvati_korisnika(this.user.user().id, this.token.accessToken()).then(
        (res: Korisnik) => {
          this.courses.set(res.kolegiji);
        },
      );
    });
  }
}
