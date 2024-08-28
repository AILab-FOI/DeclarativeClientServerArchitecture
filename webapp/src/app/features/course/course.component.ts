import { CommonModule } from '@angular/common';
import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import { DjelatnikKolegij, StudentKolegij, UserService } from '../../core';
import {
  dohvati_djelatnika_na_kolegiju,
  dohvati_studenta_na_kolegiju,
} from '../../../assets/pkg/client';
import { TokenService } from '../../core/services/token.service';
import {
  GradesComponent,
  ParticipantsComponent,
  SectionComponent,
} from '../../shared';

@Component({
  selector: 'app-course',
  standalone: true,
  imports: [
    CommonModule,
    SectionComponent,
    ParticipantsComponent,
    GradesComponent,
  ],
  templateUrl: './course.component.html',
  styleUrl: './course.component.scss',
})
export class CourseComponent {
  public user = inject(UserService);
  private token = inject(TokenService);
  public course = computed(() => {
    return this.data()?.kolegij;
  });
  public id = input.required<number>();
  public data = signal<DjelatnikKolegij | StudentKolegij>(undefined);
  public participants = computed(() => {
    return this.course().studenti;
  });
  public page = signal(0);

  constructor() {
    effect(() => {
      if (this.user.isWorker()) {
        dohvati_djelatnika_na_kolegiju(
          this.user.user().id,
          this.id(),
          this.token.accessToken(),
        ).then((res) => {
          this.data.set(res as DjelatnikKolegij);
        });
      } else {
        dohvati_studenta_na_kolegiju(
          this.user.user().id,
          this.id(),
          this.token.accessToken(),
        ).then((res) => {
          this.data.set(res as StudentKolegij);
        });
      }
    });
  }
}
