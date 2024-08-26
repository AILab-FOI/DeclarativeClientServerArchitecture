import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import { dohvati_djelatnika_na_kolegiju } from '../../../../assets/pkg/client';
import { DjelatnikKolegij, StudentKolegij, UserService } from '../../../core';
import { WorkerParticipantsComponent } from './participants/participants.component';
import { TokenService } from '../../../core/services/token.service';
import { ButtonComponent } from '../../../shared/button/button.component';
import { SectionComponent } from '../../../shared';

@Component({
  selector: 'app-worker-course',
  standalone: true,
  imports: [ButtonComponent, WorkerParticipantsComponent, SectionComponent],
  templateUrl: './course.component.html',
  styleUrl: './course.component.scss',
})
export class WorkerCourseComponent {
  public user = inject(UserService);
  private token = inject(TokenService);
  public id = input.required<number>();
  public course = computed(() => {
    return this.student_course()?.kolegij;
  });
  public student_course = signal<DjelatnikKolegij>(undefined);
  public page = signal<number>(0);

  constructor() {
    effect(() => {
      dohvati_djelatnika_na_kolegiju(
        this.user.user().id,
        this.id(),
        this.token.accessToken(),
      ).then((res) => {
        this.student_course.set(res);
      });
    });
  }

  public setPage(page: number): void {
    this.page.set(page);
  }
}
